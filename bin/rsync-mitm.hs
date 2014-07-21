{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Simple program to proxy a rsync session. E.e. a rsync client will spawn us
-- through SSH instead of a real rsync executable. It won't see any difference
-- as we are fully transparent, but we can se what the protocol looks like.
module Main (main) where

import Control.Applicative ((<$>))
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar, MVar)
import Control.Exception (finally)
import qualified Data.Attoparsec.ByteString as AB (take)
import Data.Attoparsec.ByteString hiding (take)
import Data.Bits (bit, complement, unsafeShiftL, unsafeShiftR, (.&.), (.|.), Bits)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Int (Int32)
import Data.Word (Word32)
import System.Environment (getArgs)
import System.IO (hClose, hFlush, hPutStrLn, hSetBuffering, stdin, stderr, stdout, BufferMode(..))
import System.IO.Streams (InputStream, OutputStream)
import qualified System.IO.Streams as S
import qualified System.IO.Streams.Attoparsec as S
import qualified System.Process as P

debug :: String -> IO ()
debug s = hPutStrLn stderr ("### " ++ s) >> hFlush stderr

----------------------------------------------------------------------
-- Main loop:
-- We connect our stdin, stdout, and stderr to the real rsync process
-- and feed at the same time our parser.
----------------------------------------------------------------------

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering
  args <- getArgs
  debug $ show args

  (rsyncInp, rsyncOut, rsyncErr, _) <-
    runInteractiveProcess "rsync-original" args Nothing Nothing

  (is, is') <- splitIS S.stdin
  _ <- forkIO $ protocol S.stderr ">>> " is

  inThread <- forkIO $ S.connect is' rsyncInp
  errThread <- forkIO $ S.connect rsyncErr S.stderr
  S.connect rsyncOut S.stdout
  -- takeMVar inThread
  -- takeMVar errThread

waitableForkIO :: IO () -> IO (MVar ())
waitableForkIO io = do
  mvar <- newEmptyMVar
  _ <- forkIO (io `finally` putMVar mvar ())
  return mvar

-- | Similar to S.runInteractiveProcess but set the different (wrapped)
-- Handles to NoBuffering.
runInteractiveProcess :: FilePath
  -> [String]
  -> Maybe FilePath
  -> Maybe [(String, String)]
  -> IO (OutputStream ByteString, InputStream ByteString,
    InputStream ByteString, P.ProcessHandle)
runInteractiveProcess cmd args wd env = do
    (hin, hout, herr, ph) <- P.runInteractiveProcess cmd args wd env
    hSetBuffering hin NoBuffering
    hSetBuffering hout NoBuffering
    hSetBuffering herr NoBuffering
    sIn  <- S.handleToOutputStream hin >>=
            S.atEndOfOutput (hClose hin) >>=
            S.lockingOutputStream
    sOut <- S.handleToInputStream hout >>=
            S.atEndOfInput (hClose hout) >>=
            S.lockingInputStream
    sErr <- S.handleToInputStream herr >>=
            S.atEndOfInput (hClose herr) >>=
            S.lockingInputStream
    return (sIn, sOut, sErr, ph)

splitIS :: InputStream a -> IO (InputStream a, InputStream a)
splitIS is = do
  mvar <- newEmptyMVar
  is0 <- S.makeInputStream (S.read is >>= \m -> putMVar mvar m >> return m)
  is1 <- S.makeInputStream (takeMVar mvar)
  return (is0, is1)

----------------------------------------------------------------------
-- Main io-streams parsers
----------------------------------------------------------------------

-- | `observe` is just an output stream where to show debugging info.
-- This is normally stderr. `prefix` is a few characters than can be
-- used to identify different source, typically the rsync client output
-- versus the rsync server output.
protocol :: OutputStream ByteString -> ByteString -> InputStream ByteString -> IO ()
protocol observe prefix is = do
  protocolStart is >>= showVersion prefix observe
  protocolFilterList is >>= showFilterList prefix observe
  xs_ <- protocolFileList is
  xs <- if length xs_ == 1
        then do
          ndx <- protocolAdditionalFileList is
          showNdx prefix observe ndx
          xs' <- protocolFileList is
          return $ xs_ ++ xs'
        else return xs_
  mapM_ (showFileList prefix observe . Just) xs
  bs <- S.parseFromStream (AB.take 64) is
  showUnknown prefix observe bs
  bs' <- S.parseFromStream takeByteString is
  showUnknown prefix observe bs'

protocolStart :: InputStream ByteString -> IO Int
protocolStart = S.parseFromStream parseProtocolVersion

-- | The latest protocol version is 31, from 28 Sep 2013.
-- The previous version, 30, is from 1 Mar 2008.
parseProtocolVersion :: Parser Int
parseProtocolVersion = fromIntegral <$> word32LE

-- | The filter list seems to be a list of length-prefixed bytestrings.
-- The end of the list is marked by a zero-length bytestring (i.e. a NULL
-- byte).
-- Each bytestring is a list of rules.
protocolFilterList :: InputStream ByteString -> IO [ByteString]
protocolFilterList = S.parseFromStream parseFilterList

parseFilterList :: Parser [ByteString]
parseFilterList = do
  x <- AB.take 4 -- TODO Don't know what they are.
  return [x]
{-
  n <- fromIntegral <$> word32LE
  if n == 0
    then return []
    else do
      x <- AB.take n
      xs <- parseFilterList
      return $ x : xs
-}

protocolFileList :: InputStream ByteString -> IO [(Word32, [ByteString])]
protocolFileList = S.parseFromStream $ loop []
  where loop xs = do
          m <- parseFileList
          maybe (return $ reverse xs) (loop . (:xs)) m

protocolAdditionalFileList :: InputStream ByteString -> IO Int32
protocolAdditionalFileList = S.parseFromStream readNdx

parseFileList :: Parser (Maybe (Word32, [ByteString]))
parseFileList = do
  mflags <- readFlags
  case mflags of
    Nothing -> return Nothing
    Just flags -> do
      if flags == xMIT_EXTENDED_FLAGS .|. xMIT_IO_ERROR_ENDLIST
        then error "TODO xMIT_IO_ERROR_ENDLIST"
        else do
          if flags `contains` xMIT_SAME_NAME
            then anyWord8 >> return () -- l1
            else return ()
          l2 <- fromIntegral <$> anyWord8
          thisname <- AB.take l2
          fileLength <- readVarLong 3
          modTime <- if flags `contains` xMIT_SAME_TIME then return "" else readVarLong 4
          mode <- if flags `contains` xMIT_SAME_MODE then return 0 else word32LE
          (uid, username) <-
            if flags `contains` xMIT_SAME_UID
            then return ("", "")
            else do
              a <-readVarInt
              n <- fromIntegral <$> anyWord8 -- if xMIT_USER_NAME_FOLLOWS
              u <- AB.take n
              return (a, u)
          (gid, groupname) <-
            if flags `contains` xMIT_SAME_GID
            then return ("", "")
            else do
              a <- readVarInt
              n <- fromIntegral <$> anyWord8 -- if xMIT_GROUP_NAME_FOLLOWS
              g <- AB.take n
              return (a, g)
          return $ Just (flags, [thisname, fileLength, modTime, uid, username, gid, groupname])

readFlags :: Parser (Maybe Word32)
readFlags = do
  flags <- fromIntegral <$> anyWord8
  if flags == 0
    then return Nothing
    else do
      if flags `contains` xMIT_EXTENDED_FLAGS
        then do
          flags' <- fromIntegral <$> anyWord8
          return . Just $! flags + (flags' `unsafeShiftL` 8)
        else return . Just $! flags

----------------------------------------------------------------------
-- Flags transmitted along the file list.
----------------------------------------------------------------------

-- | The first byte when transmitting the file list contains flags. If
-- xMIT_EXTENDED_FLAGS is set, the next byte also contains flags (i.e. all the
-- flags can require 16 bits instead of 8).

xMIT_SAME_MODE, xMIT_EXTENDED_FLAGS, xMIT_SAME_UID, xMIT_SAME_GID,
  xMIT_SAME_NAME, xMIT_LONG_NAME, xMIT_SAME_TIME :: Word32

xMIT_SAME_MODE = bit 1
xMIT_EXTENDED_FLAGS = bit 2
xMIT_SAME_UID = bit 3
xMIT_SAME_GID = bit 4
xMIT_SAME_NAME = bit 5
xMIT_LONG_NAME = bit 6
xMIT_SAME_TIME = bit 7

xMIT_USER_NAME_FOLLOWS, xMIT_GROUP_NAME_FOLLOWS, xMIT_IO_ERROR_ENDLIST :: Word32
xMIT_USER_NAME_FOLLOWS = bit 10
xMIT_GROUP_NAME_FOLLOWS = bit 11
xMIT_IO_ERROR_ENDLIST = bit 12

contains :: Bits a => a -> a -> Bool
contains flags flag = flags .&. flag /= 0

----------------------------------------------------------------------
-- Low-level parsers
----------------------------------------------------------------------

readVarLong :: Int -> Parser ByteString
readVarLong n = do
  b0 <- anyWord8
  bs <- AB.take $ n - 1
  let extra = extras !! (fromIntegral $ b0 `unsafeShiftR` 2)
  if extra /= 0
    then do
      let bits = bit $ 8 - extra
      bs' <- AB.take extra
      return $ bs `B.append` bs' `B.snoc` (b0 .&. (bits - 1))
    else do
      return $ bs `B.snoc` b0

readVarInt :: Parser ByteString
readVarInt = do
  b0 <- anyWord8
  let extra = extras !! (fromIntegral $ b0 `unsafeShiftR` 2)
  if extra /= 0
    then do
      let bits = bit $ 8 - extra
      bs' <- AB.take extra
      return $ bs' `B.snoc` (b0 .&. (bits - 1))
    else do
      return $ B.singleton b0

ndxDone :: Int32
ndxDone = -1

readNdx :: Parser Int32
readNdx = do
  b0 <- anyWord8
  if b0 == 0
    then return ndxDone
    else do
      (b1, sign, s) <-
        if b0 == 0xFF
        then do
          b1 <- anyWord8
          return (b1, negate, 1)
        else return (b0, id, -1)
      if b1 == 0xFE
        then do
          c0 <- anyWord8
          c1 <- anyWord8
          if c0 .&. 0x80 /= 0
            then do
              let d0 = c1
              d1 <- anyWord8
              d2 <- anyWord8
              let d3 = c0 .&. complement 0x80
              return . sign . fromIntegral $
                (fromIntegral d0 :: Word32) +
                fromIntegral d1 `unsafeShiftL` 8 +
                fromIntegral d2 `unsafeShiftL` 16 +
                fromIntegral d3 `unsafeShiftL` 24
            else do
              return . sign . fromIntegral $
                (fromIntegral c0 :: Word32) `unsafeShiftL` 8 +
                fromIntegral c1 +
                s
        else do
          return . sign . fromIntegral $
            fromIntegral b1 + s

extras :: [Int]
extras =
  [
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -- (00 - 3F)/4
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -- (40 - 7F)/4
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, -- (80 - BF)/4
        2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 5, 6 -- (C0 - FF)/4
  ]

word32LE :: Parser Word32
word32LE = do
  w1 <- anyWord8
  w2 <- anyWord8
  w3 <- anyWord8
  w4 <- anyWord8
  return $!
    (fromIntegral w1 :: Word32) +
    fromIntegral w2 `unsafeShiftL` 8 +
    fromIntegral w3 `unsafeShiftL` 16 +
    fromIntegral w4 `unsafeShiftL` 24

----------------------------------------------------------------------
-- Low-level parsers
----------------------------------------------------------------------

showVersion :: ByteString -> OutputStream ByteString -> Int -> IO ()
showVersion prefix observe a =
  S.write (Just . (prefix `BC.append`) . BC.pack $ "Protocol version: " ++ show a ++ "\n") observe

showFilterList :: ByteString -> OutputStream ByteString -> [ByteString] -> IO ()
showFilterList prefix observe a =
  S.write (Just . (prefix `BC.append`) . BC.pack $ "Filter list: " ++ show a ++ "\n") observe

showFileList :: ByteString -> OutputStream ByteString -> Maybe (Word32, [ByteString]) -> IO ()
showFileList prefix observe a =
  S.write (Just . (prefix `BC.append`) . BC.pack $ "File list: " ++ show a ++ "\n") observe

showNdx :: ByteString -> OutputStream ByteString -> Int32 -> IO ()
showNdx prefix observe a =
  S.write (Just . (prefix `BC.append`) . BC.pack $ "Ndx: " ++ show a ++ "\n") observe

showUnknown :: ByteString -> OutputStream ByteString -> ByteString -> IO ()
showUnknown prefix observe a =
  S.write (Just . (prefix `BC.append`) . BC.pack $ "Unknown: " ++ take 64 (show a) ++ "...\n") observe
