(define-library (pscheme getopt)
  (import (scheme base)
          (scheme cxr)
          (pscheme string))
  (export getopt)
  (begin

      (define (match-single grammar ch)
        (cond
         ((null? grammar) #f)
         ((char=? (cadar grammar) ch) (car grammar))
         (else (match-single (cdr grammar) ch))))

      (define (single-char-arg grammar opts rest i l)
        (cond
         ((>= i l) (inner grammar (cdr opts) rest))
         ((match-single grammar (string-ref (car opts) i)) =>
          (lambda (rule)
            (cond
             ((not (caddr rule))
              (cons (cons (car rule) #t) (single-char-arg grammar opts rest (+ 1 i) l)))
             ((= i (- l 1))
              (cons (cons (car rule) (cadr opts))
                    (inner grammar (cddr opts) rest)))
             (else
              (cons (cons (car rule) (substring (car opts) (+ i 1) l))
                    (inner grammar (cdr opts) rest))))))
         (else (error "unknown option -" (string-ref (car opts) i)))))

      (define (inner grammar opts rest)
        (cond
         ((null? opts) (list (cons 'rest (reverse rest))))
         ((string=? (car opts) "--") (list (cons rest (append (reverse rest) (cdr opts)))))
         ((string-starts-with (car opts) "--")
          (let ((rule (assoc (string->symbol (substring (car opts) 2 (string-length (car opts))))
                             grammar)))
            (cond
             ((not rule) (error "unknown option " (car opts)))
             ((caddr rule)
              (cons (cons (car rule) (cadr opts))
                    (inner grammar (cddr opts) rest)))
             (else
              (cons (cons (car rule) #t)
                    (inner grammar (cdr opts) rest))))))
         ((string-starts-with (car opts) "-")
          (single-char-arg grammar opts rest 1 (string-length (car opts))))
         (else (inner grammar (cdr opts) (cons (car opts) rest)))))

    ;; grammar is of the form ((sym #\shortform hasvalue) ...)
    ;; returns a list of the form ((key . value) ... ('rest arg ...))
    (define (getopt opts grammar)
      (inner grammar (cdr opts) '()))

    ))
