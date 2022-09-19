(define-library (pscheme compiler util)
  (import (scheme base)
          (pscheme string)
          (srfi-28))
  (export is-syntax?
          mangle-sym
          mangle
          mangle-library
          any?
          mcons
          sloppy-map
          recursive-sloppy-map
          current-unique
          unique)
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

    (define (mangle-sym sym)
      (sanitize-chars (symbol->string sym)))

    (define (mangle-library name)
      (string-join "$$" (append (map mangle-sym name))))

    (define (mangle lib sym)
      (string-append "pscm_sym_"
                     (mangle-library lib)
                     "$$"
                     (mangle-sym sym)))

    (define (any? form)
      #t)

    (define (mcons . rest)
      (if (null? (cdr rest))
          (car rest)
          (cons (car rest) (apply mcons (cdr rest)))))

    (define (sloppy-map proc lst)
      (cond
       ((null? lst) '())
       ((not (pair? lst)) (proc lst))
       (else
        (cons (proc (car lst))
              (sloppy-map proc (cdr lst))))))

    (define (recursive-sloppy-map proc lst)
      (cond
       ((null? lst) '())
       ((not (pair? lst)) (proc lst))
       (else
        (cons (recursive-sloppy-map proc (car lst))
              (recursive-sloppy-map proc (cdr lst))))))

    ;;; utils

    (define current-unique (make-parameter 0))

    (define (unique)
      (current-unique (+ 1 (current-unique)))
      (current-unique))

    ))
