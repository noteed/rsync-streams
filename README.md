# rsync-streams

Attempt to implement the rsync algorithm and protocol (i.e. be compatible with
the existing rsync executable).

Status: this can parse superficially the file list in a simple example such as:

    > rsync -az bin rsync@172.16.44.2:somedir
