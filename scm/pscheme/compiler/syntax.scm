(define-library (pscheme compiler syntax)
  (import (scheme base)
          (scheme cxr)
          (srfi 1))
  (export apply-syntax-rules)
  (begin

(define (merge-matches . matches)
  (cond
   ((null? matches) '())
   ((not (car matches)) #f)
   (else
    (let ((merged (apply merge-matches (cdr matches))))
      (and merged
           (append (car matches) merged))))))

(define (is-ellipsis? pattern)
  (and (pair? pattern)
       (pair? (cdr pattern))
       (eq? (cadr pattern) '...)))

(define-record-type match
  (make-match in-ellipsis form)
  match?
  (in-ellipsis match-in-ellipsis)
  (form match-form))

;;; matches syntax transformer patterns with ellipsis and whatnot
(define (match-syntax-pattern pattern form literals)
  (define (inner pattern form in-ellipsis)
    (cond
     ((and (null? pattern) (null? form)) '())
     ((and (is-ellipsis? pattern) (null? form)) '())
     ((member pattern literals)
      (if (eq? pattern form) '() #f))
     ((symbol? pattern) (list (cons pattern (make-match in-ellipsis form))))
     ((and (pair? pattern) (not (pair? form))) #f)
     ((pair? pattern)
      (merge-matches (inner (car pattern) (car form)
                            (or in-ellipsis (is-ellipsis? pattern)))
                     (inner (if (is-ellipsis? pattern) pattern (cdr pattern))
                            (cdr form) in-ellipsis)))
     ((equal? pattern form) '())
     (else #f)))
  (inner pattern form #f))

(define (single-match name matches)
  (define match (assoc name matches))
  (cond
   ((not match) #f)
   ((match-in-ellipsis (cdr match))
    (error "use of ellipsis match in non-ellipsis context" name))
   (else
    (cdr match))))

(define (nth-match name matches nth-rep)
  (cond
   ((null? matches) #f)
   ((not (eq? (caar matches) name))
    (nth-match name (cdr matches) nth-rep))
   ((not (match-in-ellipsis (cdar matches)))
    (cdar matches))
   ((> nth-rep 0)
    (nth-match name (cdr matches) (- nth-rep 1)))
   (else
    (cdar matches))))

; gets the nth-match if nth-rep is truthy
(define (general-match name matches nth-rep)
  (if nth-rep
      (nth-match name matches nth-rep)
      (single-match name matches)))

(define (equalize-through-error a b msg)
  (cond
   ((not (and a b)) (or a b))
   ((equal? a b) a)
   (else (error msg))))

(define (reduce f i l)
  (if (null? l)
      i
      (f (car l) (reduce f i (cdr l)))))

(define (count-matches form matches)
  (reduce + 0
          (map
           (lambda (m) (if (eq? form (car m)) 1 0))
           matches)))

(define (pattern-reps form matches)
  (cond
   ((is-ellipsis? form) #f)
   ((and (symbol? form)
         (let ((m (assoc form matches)))
           (and m (match-in-ellipsis (cdr m)))))
    (count-matches form matches))
   ((pair? form)
    (equalize-through-error
     (pattern-reps (car form) matches)
     (pattern-reps (cdr form) matches)
     "different lengths of ellipsis match in same expansion"))
   (else #f)))

(define (apply-ellipsis-pattern matches form)
  (map
   (lambda (n) (apply-syntax-pattern matches form n))
   (iota (or (pattern-reps form matches) 0))))

(define (apply-syntax-pattern matches form nth-rep)
  (cond
   ((null? form) '())
   ((symbol? form)
    (cond
     ((general-match form matches nth-rep) => match-form)
     (else form)))
   ((is-ellipsis? form)
    (append (apply-ellipsis-pattern matches (car form))
            (apply-syntax-pattern matches (cddr form) nth-rep)))
   ((pair? form)
    (cons (apply-syntax-pattern matches (car form) nth-rep)
          (apply-syntax-pattern matches (cdr form) nth-rep)))
   (else form)))

;(define (map-matches matches f)
;  (map (lambda (m) (cons (car m) (cons (cadr m) (f (cddr m)))))
;       matches))

;; takes the cdr of a syntax-rules
(define (apply-syntax-rules rules form)
  (define literals (car rules))
  (define rulepairs (cdr rules))
  (define (apply-inner rules)
    (define pattern (cdaar rules))
    (define target (cadar rules))
    (define matches (match-syntax-pattern pattern (cdr form) literals))
    (cond
     (matches (apply-syntax-pattern matches target #f))
     ((null? (cdr rules)) (error "no match for syntax-rule"))
     (else (apply-inner (cdr rules)))))
  (apply-inner rulepairs))

))
