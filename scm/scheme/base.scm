(define-library (scheme base)
  (import (pscheme ffi))
  (export
   ;; 7.3: Derived expression types
   cond case and or when unless let let* letrec letrec*
   ;; 6.1: Equivalence predicates
   eq? eqv? equal?
   ;; 6.2: Numbers
   number? complex? real? rational? integer? exact? inexact?
   exact-integer? finite? infinite? nan? = < <= > >= zero? positive?
   negative? max min + * - / quotient remainder modulo abs
   ;; 6.3: Booleans
   not boolean? boolean=?
   ;; 6.4: Pairs and Lists
   pair? cons car cdr set-car! set-cdr! caar cadr cdar cddr null? list? make-list list
   length append reverse list-tail list-ref list-set! memq memv member assq assv assoc
   ;; 6.5: Symbols
   ;; 6.6: Characters
   char? char=? char<? char>? char<=? char>=? char-ci=? char-ci<? char-ci>?
   char-ci<=? char-ci>=? char-alphabetic? char-numeric? char-whitespace?
   char-upper-case? char-lower-case? digit-value char->integer integer->char
   char-upcase char-downcase
   ;; 6.7: Strings
   string?
   procedure?
   ;; 6.13: Input and Output
   newline write-char write-string write-u8)
  (begin
    ;;; 7.3: Derived expression types

    ;; all of these are copied verbaitm from the r7rs spec

    ;; these are first, because they are used in later functions

    (define-syntax cond
      (syntax-rules (else =>)
        ((cond (else result1 result2 ...))
         (begin result1 result2 ...))
        ((cond (test => result))
         (let ((temp test))
           (if temp (result temp))))
        ((cond (test => result) clause1 clause2 ...)
         (let ((temp test))
           (if temp
               (result temp)
               (cond clause1 clause2 ...))))
        ((cond (test)) test)
        ((cond (test) clause1 clause2 ...)
         (let ((temp test))
           (if temp
               temp
               (cond clause1 clause2 ...))))
        ((cond (test result1 result2 ...))
         (if test (begin result1 result2 ...)))
        ((cond (test result1 result2 ...)
               clause1 clause2 ...)
         (if test
             (begin result1 result2 ...)
             (cond clause1 clause2 ...)))))

    (define-syntax case
      (syntax-rules (else =>)
        ((case (key ...)
           clauses ...)
         (let ((atom-key (key ...)))
           (case atom-key clauses ...)))
        ((case key
           (else => result))
         (result key))
        ((case key
           (else result1 result2 ...))
         (begin result1 result2 ...))
        ((case key
           ((atoms ...) result1 result2 ...))
         (if (memv key ’(atoms ...))
             (begin result1 result2 ...)))
        ((case key
           ((atoms ...) => result))
         (if (memv key '(atoms ...))
             (result key)))
        ((case key
           ((atoms ...) => result)
           clause clauses ...)
         (if (memv key '(atoms ...))
             (result key)
             (case key clause clauses ...)))
        ((case key
           ((atoms ...) result1 result2 ...)
           clause clauses ...)
         (if (memv key '(atoms ...))
             (begin result1 result2 ...)
             (case key clause clauses ...)))))

    (define-syntax and
      (syntax-rules ()
        ((and) #t)
        ((and test) test)
        ((and test1 test2 ...)
         (if test1 (and test2 ...) #f))))

    (define-syntax or
      (syntax-rules ()
        ((or) #f)
        ((or test) test)
        ((or test1 test2 ...)
         (let ((x test1))
           (if x x (or test2 ...))))))

    (define-syntax when
      (syntax-rules ()
        ((when test result1 result2 ...)
         (if test
             (begin result1 result2 ...)))))

    (define-syntax unless
      (syntax-rules ()
        ((unless test result1 result2 ...)
         (if (not test)
             (begin result1 result2 ...)))))

    (define-syntax let
      (syntax-rules ()
        ((let ((name val) ...) body1 body2 ...)
         ((lambda (name ...) body1 body2 ...)
          val ...))
        ((let tag ((name val) ...) body1 body2 ...)
         ((letrec ((tag (lambda (name ...)
                          body1 body2 ...)))
            tag)
          val ...))))

    (define-syntax let*
      (syntax-rules ()
        ((let* () body1 body2 ...)
         (let () body1 body2 ...))
        ((let* ((name1 val1) (name2 val2) ...)
           body1 body2 ...)
         (let ((name1 val1))
           (let* ((name2 val2) ...)
             body1 body2 ...)))))

    (define-syntax letrec
      (syntax-rules ()
        ((letrec ((var1 init1) ...) body ...)
         (letrec "generate temp names"
           (var1 ...)
           ()
           ((var1 init1) ...)
           body ...))
        ((letrec "generate temp names"
           ()
           (temp1 ...)
           ((var1 init1) ...)
           body ...)
         (let ((var1 #f) ...)
           (let ((temp1 init1) ...)
             (set! var1 temp1)
             ...
             body ...)))
        ((letrec "generate temp names"
           (x y ...)
           (temp ...)
           ((var1 init1) ...)
           body ...)
         (letrec "generate temp names"
           (y ...)
           (newtemp temp ...)
           ((var1 init1) ...)
           body ...))))

    (define-syntax letrec*
      (syntax-rules ()
        ((letrec* ((var1 init1) ...) body1 body2 ...)
         (let ((var1 #f) ...)
           (set! var1 init1)
           ...
           (let () body1 body2 ...)))))

    ;;; 6.1: Equivalence predicates

    (define (both pred a b)
      (and (pred a) (pred b)))

    (define (eq? a b)
      (builtin eq? a b))

    (define (eqv? a b)
      (or (eq? a b)
          (and (number? a) (number? b) (= a b))))

    (define (equal? a b)
      (or (eq? a b)
          (and (both number? a b) (= a b))
          (and (both pair? a b)
               (and (equal? (car a) (car b))
                    (equal? (cdr a) (cdr b))))))
          ;(and (both string? a b) (string=? a b))))

    ;;; 6.2: Numbers

    (define (number? obj)
      (or (integer? obj)))

    ;; TODO: implement these types
    (define (complex? obj) (number? obj))
    (define (real? obj) (number? obj))
    (define (rational? obj) (number? obj))

    (define (integer? obj)
      (or (builtin fixnum? obj)))

    (define (exact? obj) (integer? obj))
    (define (inexact? obj) #f)
    (define (exact-integer? obj) (integer? obj))
    (define (finite? obj) (number? obj))
    (define (infinite? obj) #f)
    (define (nan? obj) #f)

    (define (bool-fold fn first rest)
      (or (null? rest)
          (and (fn first (car rest))
               (bool-fold fn (car rest) (cdr rest)))))

    (define (= n1 n2 . rest)
      (bool-fold eq? n1 (cons n2 rest)))

    (define (< n1 n2 . rest)
      (bool-fold (lambda (a b) (builtin fixnum< a b)) n1 (cons n2 rest)))

    (define (<= n1 n2 . rest)
      (bool-fold (lambda (a b) (builtin fixnum<= a b)) n1 (cons n2 rest)))

    (define (> n1 n2 . rest)
      (bool-fold (lambda (a b) (not (builtin fixnum<= a b))) n1 (cons n2 rest)))

    (define (>= n1 n2 . rest)
      (bool-fold (lambda (a b) (not (builtin fixnum< a b))) n1 (cons n2 rest)))

    (define (zero? n) (= n 0))
    (define (positive? n) (> n 0))
    (define (negative? n) (< n 0))

    (define (order op ns curr)
      (cond
       ((null? ns) curr)
       ((op (car ns) curr) (order op (cdr ns) (car ns)))
       (else (order op (cdr ns) curr))))

    (define (max n1 . rest)
      (order > rest n1))

    (define (min n1 . rest)
      (order < rest n1))

    (define (num-fold fn acc rest)
      (if (null? rest)
          acc
          (num-fold fn (fn acc (car rest)) (cdr rest))))

    (define (+ n1 . rest)
      (num-fold (lambda (a b) (builtin fixnum+ a b)) n1 rest))

    (define (* n1 . rest)
      (num-fold (lambda (a b) (builtin fixnum* a b)) n1 rest))

    (define (- n1 . rest)
      (if (null? rest)
          (builtin fixnum- 0 n1)
          (num-fold (lambda (a b) (builtin fixnum- a b)) n1 rest)))

    (define (/ n1 . rest)
      (if (null? rest)
          (builtin fixnum/ 1 n1)
          (num-fold (lambda (a b) (builtin fixnum/ a b)) n1 rest)))

    (define (quotient n1 n2)
      (builtin fixnum/ n1 n2))

    (define (remainder n1 n2)
      (builtin fixnum-remainder n1 n2))

    (define (modulo n1 n2)
      (remainder (+ (remainder n1 n2) n2) n2))

    (define (abs n)
      (if (negative? n)
          (- n)
          n))

    ;;; 6.3: Booleans

    (define (not b)
      (if b #f #t))

    (define (boolean? obj)
      (or (eq? obj #f)
          (eq? obj #t)))

    (define (boolean=? b1 b2 . rest)
      (bool-fold eq? b1 (cons b2 rest)))

    ;;; 6.4: Pairs and lists

    (define (pair? obj)
      (builtin pair? obj))

    (define (cons a b)
      (builtin cons a b))

    (define (car obj)
      (builtin car obj))

    (define (cdr obj)
      (builtin cdr obj))

    (define (set-car! pair obj)
      (builtin set-car! pair obj))

    (define (set-cdr! pair obj)
      (builtin set-cdr! pair obj))

    (define (caar p)
      (car (car p)))

    (define (cadr p)
      (car (cdr p)))

    (define (cdar p)
      (cdr (car p)))

    (define (cddr p)
      (cdr (cdr p)))

    (define (null? obj)
      (eq? obj '()))

    (define (list? obj)
      (or (null? obj)
          (and (pair? obj)
               (list? (cdr obj)))))

    (define (make-list n . fill)
      (if (eq? n 0)
          '()
          (cons fill (make-list (- n 1) fill))))

    (define (list . rest)
      rest)

    (define (length obj)
      (if (null? obj)
          0
          (+ 1 (length (cdr obj)))))

    (define (append a b)
      (if (null? a)
          b
          (cons (car a) (append (cdr a) b))))

    (define (reverse-inner lst onto)
      (if (null? lst)
          onto
          (reverse-inner (cdr lst) (cons (car lst) onto))))

    (define (reverse lst)
      (reverse-inner lst '()))

    (define (list-tail x k)
      (if (zero? k)
          x
          (list-tail (cdr x) (- k 1))))

    (define (list-ref l k)
      (if (zero? k)
          (car l)
          (list-tail (cdr l) (- k 1))))

    (define (list-set! l k o)
      (if (zero? k)
          (set-car! l o)
          (list-tail (cdr l) (- k 1))))

    (define (memk obj lst key)
      (cond
       ((null? lst) #f)
       ((key obj (car lst)) lst)
       (else (memk obj (cdr lst) key))))

    (define (memq obj lst)
      (memk obj lst eq?))

    (define (memv obj lst)
      (memk obj lst eqv?))

    (define (member obj lst . key)
      (if (null? key)
          (memk obj lst equal?)
          (memk obj lst (car key))))

    (define (assk obj lst key)
      (cond
       ((null? lst) #f)
       ((key obj (caar lst)) (car lst))
       ( else (assk obj (cdr lst) key))))

    (define (assq obj lst)
      (assk obj lst eq?))

    (define (assv obj lst)
      (assk obj lst eqv?))

    (define (assoc obj lst . key)
      (if (null? key)
          (assk obj lst equal?)
          (assk obj lst (car key))))

    ;; 6.6: Characters

    (define (char? obj)
      (builtin char? obj))

    (define (char=? c1 c2 . rest)
      (bool-fold (lambda (a b) (= (char->integer a) (char->integer b)))
                 c1 (cons c2 rest)))

    (define (char<? c1 c2 . rest)
      (bool-fold (lambda (a b) (< (char->integer a) (char->integer b)))
                 c1 (cons c2 rest)))

    (define (char>? c1 c2 . rest)
      (bool-fold (lambda (a b) (> (char->integer a) (char->integer b)))
                 c1 (cons c2 rest)))

    (define (char<=? c1 c2 . rest)
      (bool-fold (lambda (a b) (<= (char->integer a) (char->integer b)))
                 c1 (cons c2 rest)))

    (define (char>=? c1 c2 . rest)
      (bool-fold (lambda (a b) (>= (char-foldcase (char->integer a))
                                   (char-foldcase (char->integer b))))
                 c1 (cons c2 rest)))

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
      (if (and (char>=? ch #\0) (char<=? #\9))
          (- (char->integer ch) (char->integer #\0))
          #f))

    (define (char->integer obj)
      (builtin char->integer obj))

    (define (integer->char obj)
      (builtin integer->char obj))

    (define (char-upcase ch)
      (if (and (char>=? ch #\a) (char<=? #\z))
          (integer->char (- (char->integer ch) 32))
          ch))

    (define (char-downcase ch)
      (if (and (char>=? ch #\A) (char<=? #\Z))
          (integer->char (+ (char->integer ch) 32))
          ch))

    (define (char-foldcase ch)
      (char-downcase ch))

    ;; 6.7: Strings

    (define (string? obj)
      (builtin string? obj))

    ;; 6.8: Vectors
    ;; 6.9: Byte Vectors
    ;; 6.10: Control features

    (define (procedure? obj)
      (builtin procedure? obj))


    ;; 6.13: Input and Output

    ;; 6.13.3: Output

    (define strprintf (ff->scheme void printf (char* fmt) (char* value)))
    (define chprintf (ff->scheme void printf (char* fmt) (char value)))
    (define intprintf (ff->scheme void printf (char* fmt) (int value)))

    (define (newline)
      (write-char #\newline))

    (define (write-char char)
      (chprintf "%c" char))

    (define (write-string string)
      (strprintf "%s" string))

    (define (write-u8 byte)
      (intprintf "%c" byte))

    ))
