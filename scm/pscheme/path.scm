(define-library (pscheme path)
  (import (scheme base)
          (pscheme string)
          (srfi 1))
  (export path-is-absolute?
          dirname basename)
  (begin

    (define (parts path)
      (delete "" (string-split path #\/)))

    (define (path-is-absolute? path)
      (and (> (string-length path) 0)
           (equal? (string-ref path 0) #\/)))

    (define (dirname path)
      (define s (parts path))
      (define dir (if (null? s) s (reverse (cdr (reverse s)))))
      (define abs (path-is-absolute? path))
      (cond
       ((and (null? dir) abs) "/")
       ((null? dir) ".")
       (else (string-append (if abs "/" "")
                            (string-join "/" dir)))))

    (define (basename path)
      (define s (parts path))
      (define name (if (null? s) s (car (reverse s))))
      (define abs (path-is-absolute? path))
      (cond
       ((and (null? name) abs) "/")
       ((null? name) ".")
       (else name)))

    ))
