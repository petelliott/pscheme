(define-library (pscheme compiler util)
  (import (scheme base)
          (pscheme string)
          (srfi-28))
  (export is-syntax?
          mangle
          mangle-library
          any?
          mcons)
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

    (define (sanitize-chars str)
      (map-characters (lambda (ch)
                        (cond
                         ((char=? ch #\-) "_")
                         ((or (letter? ch) (digit? ch))
                          (string ch))
                         (else (format "$~a$" (number->string (char->integer ch) 16)))))
                      str))

    (define (mangle-library name)
      (string-join "$$" (append (map (lambda (sym) (sanitize-chars (symbol->string sym)))
                                    name))))
    (define (mangle lib sym)
      (string-append "pscm_sym_"
                     (mangle-library lib)
                     "$$"
                     (sanitize-chars (symbol->string sym))))

    (define (any? form)
      #t)

    (define (mcons . rest)
      (if (null? (cdr rest))
          (car rest)
          (cons (car rest) (apply mcons (cdr rest)))))

    ))
