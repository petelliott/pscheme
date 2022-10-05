(define-library (scheme base)
  (import (pscheme ffi)
          (pscheme options))
  (export
   ;; 7.3: Derived expression types
   cond case and or when unless let let* letrec letrec* do quasiquote
   make-parameter parameterize
   ;; 5.5. Record-type definitions
   define-record-type
   ;; 6.1: Equivalence predicates
   eq? eqv? equal?
   ;; 6.2: Numbers
   number? complex? real? rational? integer? exact? inexact?
   exact-integer? finite? infinite? nan? = < <= > >= zero? positive?
   negative? max min + * - / quotient remainder modulo abs
   ;; 6.3: Booleans
   not boolean? boolean=?
   ;; 6.4: Pairs and Lists
   pair? cons car cdr set-car! set-cdr! caar cadr cdar cddr null? list?
   make-list list length append reverse list-tail list-ref list-set! memq memv
   member assq assv assoc list-copy
   ;; 6.5: Symbols
   symbol? symbol->string string->symbol
   ;; 6.6: Characters
   char? char=? char<? char>? char<=? char>=? char->integer integer->char
   ;; 6.7: Strings
   string? make-string string string-length string-ref string-set! string=?
   string<?  string>? string<=? string>=? substring string-append list->string
   string->list string-copy string-copy!
   ;; 6.8: Vectors
   vector? make-vector vector vector-length vector-ref vector-set! vector->list
   list->vector vector->string string->vector vector-copy vector-copy!
   vector-append vector-fill!
   ;; 6.9: Bytevectors
   ;; 6.10: Control features
   procedure? apply map for-each dynamic-wind
   ;; 6.11: Exceptions
   error
   ;; 6.12. Environments and evaluation
   ;; 6.13: Input and Output
   call-with-port input-port? output-port? textual-port? binary-port? port?
   input-port-open? output-port-open? current-input-port current-output-port
   current-error-port close-port close-input-port close-output-port read-char
   peek-char eof-object? eof-object read-u8 peek-u8 newline write-char
   write-string write-u8 flush-output-port
   ;; 6.14: System interface
   ;; my extensions
   make-file-port)
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
         (if (memv key '(atoms ...))
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

    (define-syntax do
      (syntax-rules ()
        ((do ((var init step ...) ...)
             (test expr ...)
           command ...)
         (letrec
             ((loop
               (lambda (var ...)
                 (if test
                     (begin
                       (if #f #f)
                       expr ...)
                     (begin
                       command
                       ...
                       (loop (do "step" var step ...)
                             ...))))))
           (loop init ...)))
        ((do "step" x)
         x)
        ((do "step" x y)
         y)))

    (define-syntax quasiquote
      (syntax-rules (unquote unquote-splicing)
        ((_ (unquote form))
         form)
        ((_ ((unquote-splicing form) . rest))
         (append form (quasiquote rest)))
        ((_ (first . rest))
         (cons (quasiquote first)
               (quasiquote rest)))
        ((_ form)
         (quote form))))

    ;; TODO: gensym
    (define <param-convert> 'ebeeb6dc-f983-425f-b30a-7865235403f0)

    (define (make-parameter init . o)
      (let* ((converter
              (if (pair? o) (car o) (lambda (x) x)))
             (value (converter init)))
        (lambda args
          (cond
           ((null? args)
            value)
           ((eq? (car args) <param-convert>)
            converter)
           (else
            (set! value (car args)))))))

    (define-syntax parameterize
      (syntax-rules ()
        ((parameterize ("step")
           ((param value p old new) ...)
           ()
           body)
         (let ((p param) ...)
           (let ((old (p)) ...
                 (new ((p <param-convert>) value)) ...)
             (dynamic-wind
               (lambda () (p new) ...)
               (lambda () . body)
               (lambda () (p old) ...)))))
        ((parameterize ("step")
           args
           ((param value) . rest)
           body)
         (parameterize ("step")
           ((param value p old new) . args)
           rest
           body))
        ((parameterize ((param value) ...) . body)
         (parameterize ("step")
           ()
           ((param value) ...)
           body))))

    ;;; 5.5. Record-type definitions

    (define (field-off flist key n)
      (if (eq? (caar flist) key)
          n
          (field-off (cdr flist) key (+ n 1))))

    (define-syntax define-record-field
      (syntax-rules ()
        ((_ flist (name accessor))
         (begin
           (define slot (field-off flist 'name 1))
           (define (accessor record) (builtin slot-ref record slot))))
        ((_ flist (name accessor modifier))
         (begin
           (define slot (field-off flist 'name 1))
           (define (accessor record) (builtin slot-ref record slot))
           (define (modifier record value) (builtin set-slot! record slot value))))))

    (define-syntax define-record-type
      (syntax-rules ()
        ((_ name (conname confields ...) predicate fields ...)
         (begin
           (define field-list '(fields ...))

           (define (conname confields ...)
             (define record (builtin alloc-slots (+ (length field-list) 1)))
             (builtin set-slot! record 0 'name)
             (builtin set-slot! record (field-off field-list 'confields 1) confields) ...
             record)

           (define (predicate obj)
             ;; this is safe because records are the only user-facing slots object availible
             (and (builtin slots? obj)
                  (eq? 'name (builtin slot-ref obj 0))))

           (define-record-field field-list fields) ...))))

    ;;; 6.1: Equivalence predicates

    (define (both pred a b)
      (and (pred a) (pred b)))

    (define (eq? a b)
      (builtin eq? a b))

    (define (eqv? a b)
      (or (eq? a b)
          (and (number? a) (number? b) (= a b))))

    (define (vector-equal?-inner a b i l)
      (if (< i l)
          (and (equal? (vector-ref a i) (vector-ref b i))
               (vector-equal?-inner a b (+ i 1) l))
          #t))

    (define (vector-equal? a b)
      (and (equal? (vector-length a) (vector-length b))
           (vector-equal?-inner a b 0 (vector-length a))))

    (define (equal? a b)
      (or (eqv? a b)
          (and (both pair? a b)
               (and (equal? (car a) (car b))
                    (equal? (cdr a) (cdr b))))
          (and (both vector? a b)
               (vector-equal? a b))
          (and (both string? a b) (string=? a b))))

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

    (define (make-list n . rest)
      (options rest (fill #f))
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
          (list-ref (cdr l) (- k 1))))

    (define (list-set! l k o)
      (if (zero? k)
          (set-car! l o)
          (list-set! (cdr l) (- k 1) o)))

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

    (define (list-copy lst)
      (if (null? lst)
          '()
          (cons (car lst)
                (list-copy (cdr lst)))))

    ;;; 6.5: Symbols

    (define (symbol? obj)
      (builtin symbol? obj))

    (define (symbol->string obj)
      (builtin symbol->string obj))

    ;; TODO: intern symbols
    (define (string->symbol obj)
      (builtin string->symbol obj))

    ;;; 6.6: Characters

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
      (bool-fold (lambda (a b) (>= (char->integer a) (char->integer b)))
                 c1 (cons c2 rest)))

    (define (char->integer obj)
      (builtin char->integer obj))

    (define (integer->char obj)
      (builtin integer->char obj))

    ;;; 6.7: Strings

    ;; some string.h functions that we use
    (define memset (ff->scheme void memset (char* dst) (char c) (int n)))
    (define strlen (ff->scheme int strlen (char* str)))
    (define strcmp (ff->scheme int strcmp (char* s1) (char* s2)))

    (define (string? obj)
      (builtin string? obj))

    (define (make-string k . fill)
      (define str (builtin alloc-string (+ k 1)))
      (string-set! str k #\null)
      (unless (null? fill)
        (memset str (car fill) k))
      str)

    (define (string . chars)
      (list->string chars))

    (define (string-length string)
      (strlen string))

    (define (string-ref string k)
      (builtin string-ref string k))

    (define (string-set! string k char)
      (builtin string-set! string k char)
      (begin))

    (define (string=? s1 s2 . rest)
      (bool-fold (lambda (a b) (= (strcmp a b) 0))
                 s1 (cons s2 rest)))

    (define (string<? s1 s2 . rest)
      (bool-fold (lambda (a b) (< (strcmp a b) 0))
                 s1 (cons s2 rest)))

    (define (string>? s1 s2 . rest)
      (bool-fold (lambda (a b) (> (strcmp a b) 0))
                 s1 (cons s2 rest)))

    (define (string<=? s1 s2 . rest)
      (bool-fold (lambda (a b) (<= (strcmp a b) 0))
                 s1 (cons s2 rest)))

    (define (string>=? s1 s2 . rest)
      (bool-fold (lambda (a b) (>= (strcmp a b) 0))
                 s1 (cons s2 rest)))

    (define (substring string start end)
      (define new (make-string length))
      (builtin strcpy new string (- end start) 0 start)
      new)

    ; TODO: write fold
    (define (append-length l)
      (if (null? l)
          0
          (+ (string-length (car l))
             (append-length (cdr l)))))

    (define (string-append . strings)
      (define new (make-string (append-length strings)))
      (define off 0)
      (for-each (lambda (string)
                  (define l (string-length string))
                  (builtin strcpy new string l off 0)
                  (set! off (+ off l)))
                strings)
      new)

    (define (string->list-inner string start end)
      (if (>= start end)
          '()
          (cons (string-ref string start)
                (string->list-inner string (+ start 1) end))))

    (define (string->list string . args)
      (options args (start 0) (end (string-length string)))
      (string->list-inner string start end))

    (define (list->string-inner i list string)
      (unless (null? list)
        (string-set! string i (car list))
        (list->string-inner (+ i 1) (cdr list) string)))

    (define (list->string list)
      (define string (make-string (length list)))
      (list->string-inner 0 list string)
      string)

    (define (string-copy string . args)
      (options args (start 0) (end (string-length string)))
      (substring string start end))

    (define (string-copy! to at from . args)
      (options args (start 0) (end (string-length from)))
      (builtin strcpy to from (- end start) at start))

    ;; TODO: string-fill!

    ;;; 6.8: Vectors

    (define-record-type vector
      (construct-vector length data)
      vector?
      (length vector-length vector-set-length!)
      (data vector-data vector-set-data!))

    (define (make-vector k . fill)
      (define vector (construct-vector k (builtin alloc-slots k)))
      (unless (null? fill)
        (vector-fill! vector (car fill)))
      vector)

    (define (vector . rest)
      (list->vector rest))

    (define (vector-ref vector k)
      (builtin slot-ref (vector-data vector) k))

    (define (vector-set! vector k obj)
      (builtin set-slot! (vector-data vector) k obj))

    (define (vector->list vector . rest)
      (options rest (start 0) (end (vector-length vector)))
      (define (inner i)
        (if (< i end)
            (cons (vector-ref vector i)
                  (inner (+ i 1)))
            '()))
      (inner start))

    (define (list->vector list)
      (do ((vector (make-vector (length list)))
           (l list (cdr l))
           (i 0 (+ i 1)))
          ((null? l) vector)
        (vector-set! vector i (car l))))

    (define (vector->string vector . rest)
      (options rest (start 0) (end (vector-length vector)))
      (do ((str (make-string (- end start)))
           (i start (+ i 1)))
          ((>= i end) str)
        (string-set! str (- i start) (vector-ref vector i))))

    (define (string->vector string . rest)
      (options rest (start 0) (end (string-length string)))
      (do ((vector (make-vector (- end start)))
           (i start (+ i 1)))
          ((>= i end) vector)
        (vector-set! vector (- i start) (string-ref string i))))

    (define (vector-copy vector0 . rest)
      (options rest (start 0) (end (vector-length vector0)))
      (do ((vector1 (make-vector (- end start)))
           (i start (+ i 1)))
          ((>= i end) vector1)
        (vector-set! vector1 (- i start) (vector-ref vector0 i))))

    ;; TODO: memmove semantics
    (define (vector-copy! to at from . rest)
      (options rest (start 0) (end (vector-length from)))
      (do ((i start (+ i 1)))
          ((>= i end) to)
        (vector-set! to (+ at (- i start)) (vector-ref from i))))

    (define (vector-append . vectors)
      (define new-len (apply + (map vector-length vectors)))
      (define new-vec (make-vector new-len))
      (define off 0)
      (do ((v vectors (cdr v)))
          ((null? v))
        (vector-copy! new-vec off (car v))
        (set! off (+ off (vector-length (car v)))))
      new-vec)

    (define (vector-fill! vector fill . rest)
      (options rest (start 0) (end (vector-length vector)))
      (do ((i start (+ i 1)))
          ((>= i end))
        (vector-set! vector i fill))
      vector)

    ;;; 6.9: Byte Vectors
    ;;; 6.10: Control features

    (define (procedure? obj)
      (builtin procedure? obj))

    (define (process-apply-args args)
      (if (null? (cdr args))
          (car args)
          (cons (car args) (process-apply-args (cdr args)))))

    (define (apply proc . args)
      (define pargs (process-apply-args args))
      (builtin ffi-call
               (ffi-symbol pscheme_internal_apply)
               proc
               (builtin fixnum->ffi (length pargs))
               pargs))

    (define (any-null? list)
      (if (null? list)
          #f
          (or (null? (car list))
              (any-null? (cdr list)))))

    (define (map1 proc list)
      (if (null? list)
          '()
          (cons (proc (car list))
                (map1 proc (cdr list)))))

    (define (map-inner proc lists)
      (if (any-null? lists)
          '()
          (cons (apply proc (map1 car lists))
                (map-inner proc (map1 cdr lists)))))

    (define (map proc list1 . lists)
      (if (null? lists)
          (map1 proc list1) ; fast path
          (map-inner proc (cons list1 lists))))

    (define (for-each1 proc list)
      (unless (null? list)
        (proc (car list))
        (for-each1 proc (cdr list))))

    (define (for-each-inner proc lists)
      (unless (any-null? lists)
        (apply proc (map1 car lists))
        (for-each-inner proc (map1 cdr lists))))

    (define (for-each proc list1 . lists)
      (if (null? lists)
          (for-each1 proc list1)
          (for-each-inner proc (cons list1 lists))))

    ;; TODO: an acutal dynamic-wind
    (define (dynamic-wind before thunk after)
      (before)
      (let ((v (thunk)))
        (after)
        v))

    ;;; 6.11: Exceptions

    (define (error message . objs)
      ;; TODO: this is obviously fake lol
      (write-string message (current-error-port))
      (newline (current-error-port))
      (builtin ffi-call (ffi-symbol abort)))

    ;;; 6.12. Environments and evaluation
    ;;; 6.13: Input and Output

    ;;; 6.13.1: Ports

    (define-record-type port-impl
      (make-port-impl read-char peek-char read-u8 peek-u8 write-char write-u8 write-string flush close)
      port-impl?
      (read-char port-impl-read-char)
      (peek-char port-impl-peek-char)
      (read-u8 port-impl-read-u8)
      (peek-u8 port-impl-peek-u8)
      (write-char port-impl-write-char)
      (write-u8 port-impl-write-u8)
      (write-string port-impl-write-string)
      (flush port-impl-flush)
      (close port-impl-close))

    (define (file-read-u8 file*)
      (define num (builtin ffi->fixnum (builtin ffi-call (ffi-symbol fgetc) file*)))
      (if (= num -1)
          (eof-object)
          num))

    (define (file-peek-u8 file*)
      (define i (file-read-u8 file*))
      (if (eof-object? i)
          i
          (builtin ffi-call (ffi-symbol ungetc) (builtin fixnum->ffi i) file*)))

    (define (file-read-char file*)
      (define i (file-read-u8 file*))
      (if (eof-object? i) i (integer->char i)))

    (define (file-peek-char file*)
      (define i (file-peek-u8 file*))
      (if (eof-object? i) i (integer->char i)))

    (define (file-write-u8 file* i)
      (builtin ffi-call (ffi-symbol fputc) (builtin fixnum->ffi i) file*))

    (define (file-write-char file* c)
      (file-write-u8 file* (char->integer c)))

    (define (file-write-string file* string)
      (builtin ffi-call (ffi-symbol fputs) (builtin string->ffi string) file*))

    (define (file-flush file*)
      (builtin ffi-call (ffi-symbol fflush) file*))

    (define (file-close file*)
      (builtin ffi-call (ffi-symbol fclose) file*))

    (define port-file-impl
      (make-port-impl
       file-read-char
       file-peek-char
       file-read-u8
       file-peek-u8
       file-write-char
       file-write-u8
       file-write-string
       file-flush
       file-close))

    (define (make-file-port file input-open output-open)
      (make-port port-file-impl file input-open output-open))

    (define-record-type port
      (make-port impl data input-open output-open)
      port?
      (impl port-impl)
      (data port-data)
      (input-open input-port-open? set-input-port-open!)
      (output-open output-port-open? set-output-port-open!))

    (define (call-with-port port proc)
      (proc)
      (close-port port))

    (define input-port? port?)
    (define output-port? port?)
    (define textual-port? port?)
    (define binary-port? port?)

    (define current-input-port
      (make-parameter (make-file-port (builtin ffi-call (ffi-symbol pscheme_default_input_port_file)) #t #f)))
    (define current-output-port
      (make-parameter (make-file-port (builtin ffi-call (ffi-symbol pscheme_default_output_port_file)) #f #t)))
    (define current-error-port
      (make-parameter (make-file-port (builtin ffi-call (ffi-symbol pscheme_default_error_port_file)) #f #t)))

    (define (close-port port)
      (define impl (port-impl port))
      (set-input-port-open! port #f)
      (set-output-port-open! port #f)
      ((port-impl-close (port-impl port)) (port-data port)))

    (define (close-input-port port)
      (set-input-port-open! port #f)
      (when (not (or (input-port-open? port)
                     (output-port-open? port)))
        (close-port port)))

    (define (close-output-port port)
      (set-output-port-open! port #f)
      (when (not (or (input-port-open? port)
                     (output-port-open? port)))
        (close-port port)))

    ;;; 6.13.2: Input

    (define (eof-object? obj)
      (eq? obj (builtin eof-object)))

    (define (eof-object)
      (builtin eof-object))

    (define (read-char . rest)
      (options rest (port (current-input-port)))
      ((port-impl-read-char (port-impl port)) (port-data port)))

    (define (peek-char . rest)
      (options rest (port (current-input-port)))
      ((port-impl-peek-char (port-impl port)) (port-data port)))

    (define (read-u8 . rest)
      (options rest (port (current-input-port)))
      ((port-impl-read-u8 (port-impl port)) (port-data port)))

    (define (peek-u8 . rest)
      (options rest (port (current-input-port)))
      ((port-impl-peek-u8 (port-impl port)) (port-data port)))

    ;;; 6.13.3: Output

    (define (newline . rest)
      (options rest (port (current-output-port)))
      (write-char #\newline port))

    (define (write-char char . rest)
      (options rest (port (current-output-port)))
      ((port-impl-write-char (port-impl port)) (port-data port) char)
      (begin))

    (define (write-string string . rest)
      (options rest (port (current-output-port)))
      ((port-impl-write-string (port-impl port)) (port-data port) string)
      (begin))

    (define (write-u8 byte . rest)
      (options rest (port (current-output-port)))
      ((port-impl-write-u8 (port-impl port)) (port-data port) byte)
      (begin))

    (define (flush-output-port . rest)
      (options rest (port (current-output-port)))
      ((port-impl-flush (port-impl port)) (port-data port))
      (begin))

    ))
