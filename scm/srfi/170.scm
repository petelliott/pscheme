(define-library (srfi 170)
  (import (scheme base)
          (pscheme options)
          (pscheme ffi))
  (export file-info?
          file-info:device file-info:inode file-info:mode file-info:nlinks
          file-info:uid file-info:gid file-info:rdev file-info:size file-info:blksize
          file-info:blocks file-info:atime file-info:mtime file-info:ctime
          file-info)
  (begin

    (define-record-type file-info
      (make-file-info)
      file-info?
      (device file-info:device)
      (inode file-info:inode)
      (mode file-info:mode)
      (nlinks file-info:nlinks)
      (uid file-info:uid)
      (gid file-info:gid)
      (rdev file-info:rdev)
      (size file-info:size)
      (blksize file-info:blksize)
      (blocks file-info:blocks)
      (atime file-info:atime)
      (mtime file-info:mtime)
      (ctime file-info:ctime))


    (define (file-info string follow?)
      (define fi (make-file-info))
      (builtin ffi-call (ffi-symbol pscheme_populate_file_info)
               (builtin string->ffi string) fi
               (if follow? (builtin fixnum->ffi 1) (builtin fixnum->ffi 0)))
      fi)

    ))
