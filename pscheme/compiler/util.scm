(define-library (pscheme compiler util)
  (import (scheme base)
          (srfi-28))
  (export is-syntax?
          mangle)
  (begin

    (define (is-syntax? sym form)
      (and (pair? form)
           (eq? (car form) sym)))


    (define (map-characters proc str)
      (apply string-append
             (map proc (string->list str))))

    (define (letter? ch)
      (or (and (char>=? ch #\a) (char<=? ch #\z))
          (and (char>=? ch #\A) (char<=? ch #\Z))))

    (define (digit? ch)
      (and (char>=? ch #\0) (char<=? ch #\9)))

    (define (mangle sym)
      (string-append "pscm_sym_"
                     (map-characters
                      (lambda (ch)
                        (cond
                         ((char=? ch #\-) "_")
                         ((or (letter? ch) (digit? ch) (char=? ch #\.))
                          (string ch))
                         (else (format "$~a$" (number->string (char->integer ch) 16)))))
                      (symbol->string sym))))


    ))
