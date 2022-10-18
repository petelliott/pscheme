(import (scheme base)
        (pscheme test))

;;; 6.1: Equivalence predicates

(define-test "eqv?"
  ;; true cases
  (assert (eqv? #t #t))
  (assert (eqv? #f #f))
  (assert (eqv? 'hello 'hello))
  (assert (eqv? (+ 1 2) (+ 1 2)))
  ;; TODO: non-fixnums
  (assert (eqv? #\a #\a))
  (assert (eqv? '() '()))
  (define a '(1 2 3))
  (assert (eqv? a a))
  (define b "hello world")
  (assert (eqv? b b))
  ;; TODO: vectors, bytevectors, records
  (define (f x) x)
  (assert (eqv? f f))
  ;; false cases
  (assert (not (eqv? 1 '())))
  (assert (not (eqv? #t #f)))
  (assert (not (eqv? 'hello 'world)))
  ;; TODO: exact vs inexact
  (assert (not (eqv? 1 2)))
  ;; TODO: NaN
  (assert (not (eqv? #\a #\b)))
  (assert (not (eqv? '() 8)))
  (assert (not (eqv? '(1 2 3) '(1 2 3))))
  (assert (not (eqv? "hello world" "hello world"))))


(define-test "eq?"
  ;; true cases
  (assert (eq? #t #t))
  (assert (eq? #f #f))
  (assert (eq? 'hello 'hello))
  (assert (eq? '() '()))
  (define a '(1 2 3))
  (assert (eq? a a))
  (define b "hello world")
  (assert (eq? b b))
  ;; TODO: vectors, bytevectors, records
  (define (f x) x)
  (assert (eq? f f))
  ;; false cases
  (assert (not (eq? 1 '())))
  (assert (not (eq? #t #f)))
  (assert (not (eq? 'hello 'world)))
  (assert (not (eq? '() 8)))
  (assert (not (eq? '(1 2 3) '(1 2 3))))
  (assert (not (eq? "hello world" "hello world"))))

(define-test "equal?"
  ;; true cases
  (assert (equal? 'a 'a))
  (assert (equal? '(a) '(a)))
  (assert (equal? '(a (b) c) '(a (b) c)))
  (assert (equal? "abc" "abc"))
  (assert (equal? 2 2))
  (assert (equal? (vector) (vector)))
  (assert (equal? (vector 5 'a) (vector 5 'a)))
  ;; TODO: circular forms
  (assert (not (equal? 'a 'b)))
  (assert (not (equal? '(a) '(b))))
  (assert (not (equal? '(a (b) c) '(a (d) c))))
  (assert (not (equal? "abc" "abcd")))
  (assert (not (equal? 2 3)))
  (assert (not (equal? (vector 5 'a) (vector 5 'b)))))

;;; 6.2: Numbers

(define-test "all number tests lol" SKIP)

;;; 6.3: Booleans

(define-test "not"
  (assert (not #f))
  (assert (not (not #t))))

(define-test "boolean?"
  (assert (boolean? #t))
  (assert (boolean? #f))
  (assert (not (boolean? '())))
  (assert (not (boolean? 0))))

(define-test "boolean=?"
  (assert (boolean=? #t #t #t))
  (assert (boolean=? #f #f #f))
  (assert (not (boolean=? #f #t #f))))

;;; 6.4: Pairs and lists

(define-test "pair?"
  (assert (pair? '(1 . 2)))
  (assert (pair? '(1 2 3 4 5)))
  (assert (not (pair? '()))))

(define-test "cons"
  (assert (pair? (cons 1 2)))
  (assert (equal? (cons 1 2) '(1 . 2)))
  (assert (equal? (cons 1 '(2 3)) '(1 2 3))))

(define-test "car"
  (assert (equal? (car '(1 . 2)) 1))
  (assert (equal? (car '((1) 2 3)) '(1))))

(define-test "cdr"
  (assert (equal? (cdr '(1 . 2)) 2))
  (assert (equal? (cdr '((1) 2 3)) '(2 3))))

(define-test "set-car!"
  (define a '(1 2 3))
  (assert (equal? (car a) 1))
  (set-car! a 8)
  (assert (equal? (car a) 8)))

(define-test "set-cdr!"
  (define a '(1 2 3))
  (assert (equal? (cdr a) '(2 3)))
  (set-cdr! a 8)
  (assert (equal? (cdr a) 8)))

(define-test "caar"
  (assert (equal? (caar '((1 . 2) . (3 . 4))) 1)))
(define-test "cadr"
  (assert (equal? (cadr '((1 . 2) . (3 . 4))) 3)))
(define-test "cdar"
  (assert (equal? (cdar '((1 . 2) . (3 . 4))) 2)))
(define-test "cddr"
  (assert (equal? (cddr '((1 . 2) . (3 . 4))) 4)))

(define-test "null?"
  (assert (null? '()))
  (assert (not (null? '(1)))))

(define-test "list?"
  (assert (list? '(a b c)))
  (assert (list? '()))
  (assert (not (list? '(a . b))))
  (assert (not (list? 5))))

(define-test "make-list"
  (assert (equal? (make-list 0) '()))
  (assert (equal? (length (make-list 5)) 5))
  (assert (equal? (make-list 0 '(a)) '()))
  (assert (equal? (make-list 5 '(a)) '((a) (a) (a) (a) (a)))))

(define-test "list"
  (assert (equal? (list) '()))
  (assert (equal? (list 1 2 (+ 1 2)) '(1 2 3))))

(define-test "length"
  (assert (equal? (length '(1 2 3 4)) 4))
  (assert (equal? (length '()) 0)))

(define-test "append"
  (assert (equal? (append '(1 2 3 4) '()) '(1 2 3 4)))
  (assert (equal? (append '() '(1 2 3 4)) '(1 2 3 4)))
  (assert (equal? (append '(1 2) '(3 4)) '(1 2 3 4))))

(define-test "reverse"
  (assert (equal? (reverse '()) '()))
  (assert (equal? (reverse '(1 2 3 4)) '(4 3 2 1))))

(define-test "list-tail"
  (assert (equal? (list-tail '(1 2 3 4) 0) '(1 2 3 4)))
  (assert (equal? (list-tail '(1 2 3 4) 2) '(3 4)))
  (assert (equal? (list-tail '(1 2 3 4) 4) '())))

(define-test "list-ref"
  (assert (equal? (list-ref '(1 2 3 4) 0) 1))
  (assert (equal? (list-ref '(1 2 3 4) 1) 2))
  (assert (equal? (list-ref '(1 2 3 4) 2) 3))
  (assert (equal? (list-ref '(1 2 3 4) 3) 4)))

(define-test "list-set!"
  (define a '(1 2 3 4))
  (assert (equal? a '(1 2 3 4)))
  (list-set! a 1 5)
  (assert (equal? a '(1 5 3 4))))

(define-test "memq"
  (assert (equal? (memq 'a '(a b c)) '(a b c)))
  (assert (equal? (memq 'b '(a b c)) '(b c)))
  (assert (equal? (memq 'd '(a b c)) #f)))

(define-test "memv"
  (assert (equal? (memv 'a '(a b c)) '(a b c)))
  (assert (equal? (memv 'b '(a b c)) '(b c)))
  (assert (equal? (memv 'd '(a b c)) #f)))

(define-test "member"
  (assert (equal? (member '(a) '((a) (b) (c))) '((a) (b) (c))))
  (assert (equal? (member '(b) '((a) (b) (c))) '((b) (c))))
  (assert (equal? (member '(d) '((a) (b) (c))) #f))

  (assert (equal? (member 'a '() (lambda (i x) #t)) #f))
  (assert (equal? (member 'a '(1 2 3) (lambda (i x) #t)) '(1 2 3))))

(define-test "assq"
  (assert (equal? (assq 'a '((a . 1) (b . 2))) '(a . 1)))
  (assert (equal? (assq 'b '((a . 1) (b . 2))) '(b . 2)))
  (assert (equal? (assq 'c '((a . 1) (b . 2))) #f)))

(define-test "assv"
  (assert (equal? (assv 'a '((a . 1) (b . 2))) '(a . 1)))
  (assert (equal? (assv 'b '((a . 1) (b . 2))) '(b . 2)))
  (assert (equal? (assv 'c '((a . 1) (b . 2))) #f)))

(define-test "assoc"
  (assert (equal? (assoc '(a) '(((a) . 1) ((b) . 2))) '((a) . 1)))
  (assert (equal? (assoc '(b) '(((a) . 1) ((b) . 2))) '((b) . 2)))
  (assert (equal? (assoc '(c) '(((a) . 1) ((b) . 2))) #f))
  (assert (equal? (assoc 'c '() (lambda (i x) #t)) #f))
  (assert (equal? (assoc 'c '((a . 1) (b . 2)) (lambda (i x) #t)) '(a . 1))))

(define-test "list-copy"
  (define a '(1 2 3))
  (define b (list-copy a))
  (assert (not (eq? a b)))
  (assert (equal? a b)))

;; 6.5: Symbols

(define-test "symbol?"
  (assert (symbol? 'hello))
  (assert (not (symbol? "hello")))
  (assert (not (symbol? 5))))

(define-test "symbol->string"
  (assert (string? (symbol->string 'hello)))
  (assert (equal? (symbol->string 'hello) "hello")))

(define-test "string->symbol"
  (assert (symbol? (string->symbol "hello")))
  (assert (eq? (string->symbol "hello") 'hello)))

;;; 6.7: Strings

(define-test "string?"
  (assert (string? "hello world")))

(define-test "make-string"
  (assert (string? (make-string 10)))
  (assert (string? (make-string 10 #\a)))
  (assert (equal? "aaa" (make-string 3 #\a))))

(define-test "string"
  (assert (string? (string #\a #\b #\c)))
  (assert (equal? "abc" (string #\a #\b #\c))))

(define-test "string-length"
  (assert (equal? (string-length "123456") 6)))

(define-test "string-ref"
  (assert (equal? (string-ref "0123" 0) #\0))
  (assert (equal? (string-ref "0123" 1) #\1))
  (assert (equal? (string-ref "0123" 2) #\2))
  (assert (equal? (string-ref "0123" 3) #\3)))

;;; 6.8: Vectors

(define-test "vector?"
  (assert (vector? (make-vector 0)))
  (assert (not (vector? '()))))

(define-test "make-vector"
  (define v (make-vector 5 8))
  (assert (vector? v))
  (assert (equal? (vector-length v) 5))
  (assert (equal? (vector-ref v 4) 8)))

(define-test "vector"
  (define v (vector 'a 'b 'c))
  (assert (vector? v))
  (assert (equal? (vector-length v) 3))
  (assert (eq? (vector-ref v 0) 'a))
  (assert (eq? (vector-ref v 1) 'b))
  (assert (eq? (vector-ref v 2) 'c)))

(define-test "list->vector"
  (define l '(1 () (1 2 3) a))
  (assert (vector? (list->vector l)))
  (assert (list? (vector->list (list->vector l))))
  (assert (equal? (vector-length (list->vector '())) 0))
  (assert (equal? (vector-length (list->vector l)) 4))
  (assert (equal? (vector->list (list->vector l)) l)))

(define-test "vector->list"
  (define v (vector 1 '() 'a))
  (define l0 (vector->list v 2 2))
  (define l1 (vector->list v 1 2))
  (define l2 (vector->list v 1))
  (assert (list? l0))
  (assert (equal? l0 '()))
  (assert (list? l1))
  (assert (equal? l1 '(())))
  (assert (list? l2))
  (assert (equal? l2 '(() a))))

(define-test "vector->string"
  (define v (vector #\h #\e #\l #\l #\o))
  (assert (equal? (vector->string v) "hello"))
  (assert (equal? (vector->string v 2) "llo"))
  (assert (equal? (vector->string v 1 4) "ell")))

(define-test "string->vector"
  (define s "hello")
  (assert (equal? (string->vector s) (vector #\h #\e #\l #\l #\o)))
  (assert (equal? (string->vector s 2) (vector #\l #\l #\o)))
  (assert (equal? (string->vector s 1 4) (vector #\e #\l #\l))))

(define-test "vector-copy"
  (define v (vector #\h #\e #\l #\l #\o))
  (assert (equal? (vector-copy v) (vector #\h #\e #\l #\l #\o)))
  (assert (equal? (vector-copy v 2) (vector #\l #\l #\o)))
  (assert (equal? (vector-copy v 1 4) (vector #\e #\l #\l))))

(define-test "vector-copy!"
  (define v1 (vector 1 2 3 4 5))
  (define v2 (vector 'a 'b 'c 'd 'e))
  (assert (equal? (vector-copy! v2 1 v1 1 4) (vector 'a 2 3 4 'e)))
  (assert (equal? v2 (vector 'a 2 3 4 'e)))
  (assert (equal? (vector-copy! v2 0 v1) (vector 1 2 3 4 5)))
  (assert (equal? v2 (vector 1 2 3 4 5))))

(define-test "vector-append"
  (assert (equal? (vector-append (vector) (vector 1 2) (vector 3 4)) (vector 1 2 3 4))))

(define-test "vector-fill!"
  (define v1 (vector 1 2 3 4 5))
  (vector-fill! v1 '(1))
  (assert (equal? v1 (vector '(1) '(1) '(1) '(1) '(1)))))

;;; 7.3: Derived expression types

(define-test "cond"
  (assert (equal? (cond (#t 1) (#t 2) (#t 3)) 1))
  (assert (equal? (cond (#f 1) (8 2) (#t 3)) 2))
  (assert (equal? (cond (#f 1) (#f 2) ('() 3)) 3))
  (assert (equal? (cond (#f 1) (#f 2) (#f 3)) (begin)))
  (assert (equal? (cond (#f 1) (#f 2) (#t 3) (else 4)) 3))
  (assert (equal? (cond (#f 1) (#f 2) (#f 3) (else 4)) 4))
  (assert (equal? (cond (#f 1) ((+ 1 2) => (lambda (a) (+ a 5)))) 8)))

(define-test "case"
  (assert (equal? (case (+ 1 2) ((2) 1) ((4 3 8) 2) (else 3)) 2))
  (assert (equal? (case (+ 1 64) ((2) 1) ((4 3 8) 2) (else 3)) 3)))

(define-test "and"
  (assert (equal? (and 7 8 (+ 1 2)) 3))
  (assert (equal? (and 7 #f (+ 1 2)) #f))
  (assert (equal? (and 1) 1))
  (assert (equal? (and) #t)))

(define-test "or"
  (assert (equal? (or 7 8 (+ 1 2)) 7))
  (assert (equal? (or #f #f (+ 1 2)) 3))
  (assert (equal? (or 1) 1))
  (assert (equal? (or) #f)))

(define-test "when"
  (assert (equal? (when #t 1 2 (+ 1 2)) 3))
  (assert (equal? (when #f 1 2 (+ 1 2)) (begin))))

(define-test "unless"
  (assert (equal? (unless #t 1 2 (+ 1 2)) (begin)))
  (assert (equal? (unless #f 1 2 (+ 1 2)) 3)))

(define-test "let"
  (assert (equal? (let ((a (+ 1 2))) 1 a) 3))
  (assert (equal? (let ((a 1) (b 2)) (+ a b)) 3)))

(define-test "let*"
  (assert (equal? (let* ((a (+ 1 2))) 1 a) 3))
  (assert (equal? (let* ((a 1) (b (+ a 1))) (+ a b)) 3)))

(define-test "letrec" SKIP)
(define-test "letrec*" SKIP)

(define-test "let-values" SKIP)
(define-test "let*-values" SKIP)
(define-test "define-values" SKIP)

(define-test "do" SKIP)

(define-test "promises" SKIP)
(define-test "parameters" SKIP)

(define-test "guard" SKIP)
(define-test "cond-expand" SKIP)


(finish-tests)
