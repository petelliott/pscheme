(define-library (scheme char)
  (import (scheme base)
          (pscheme ffi))
  (export
   char-ci=? char-ci<? char-ci>?  char-ci<=? char-ci>=? char-alphabetic?
   char-numeric? char-whitespace?  char-upper-case? char-lower-case? digit-value
   char-upcase char-downcase char-foldcase)
  (begin

    (define (bool-fold fn first rest)
      (or (null? rest)
          (and (fn first (car rest))
               (bool-fold fn (car rest) (cdr rest)))))

    (define (char-ci=? c1 c2 . rest)
      (bool-fold (lambda (a b) (= (char-foldcase (char->integer a))
                                  (char-foldcase (char->integer b))))
                 c1 (cons c2 rest)))

    (define (char-ci<? c1 c2 . rest)
      (bool-fold (lambda (a b) (< (char-foldcase (char->integer a))
                                  (char-foldcase (char->integer b))))
                 c1 (cons c2 rest)))

    (define (char-ci>? c1 c2 . rest)
      (bool-fold (lambda (a b) (> (char-foldcase (char->integer a))
                                  (char-foldcase (char->integer b))))
                 c1 (cons c2 rest)))

    (define (char-ci<=? c1 c2 . rest)
      (bool-fold (lambda (a b) (<= (char-foldcase (char->integer a))
                                   (char-foldcase (char->integer b))))
                 c1 (cons c2 rest)))

    (define (char-ci>=? c1 c2 . rest)
      (bool-fold (lambda (a b) (>= (char-foldcase (char->integer a))
                                   (char-foldcase (char->integer b))))
                 c1 (cons c2 rest)))

    (define char-alphabetic? (ff->scheme bool isalpha (char c)))
    (define char-numeric? (ff->scheme bool isdigit (char c)))
    (define char-whitespace? (ff->scheme bool isspace (char c)))
    (define char-upper-case? (ff->scheme bool isupper (char c)))
    (define char-lower-case? (ff->scheme bool islower (char c)))

    (define (digit-value ch)
      (if (and (char>=? ch #\0) (char<=? ch #\9))
          (- (char->integer ch) (char->integer #\0))
          #f))

    (define (char-upcase ch)
      (if (and (char>=? ch #\a) (char<=? ch #\z))
          (integer->char (- (char->integer ch) 32))
          ch))

    (define (char-downcase ch)
      (if (and (char>=? ch #\A) (char<=? ch #\Z))
          (integer->char (+ (char->integer ch) 32))
          ch))

    (define (char-foldcase ch)
      (char-downcase ch))

    ))
