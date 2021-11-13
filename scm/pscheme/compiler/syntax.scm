;;(define-library (pscheme compiler syntax)
;;  (import (scheme base))
;;  (export)
;;  (begin

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

;;; matches syntax transformer patterns with ellipsis and whatnot
(define (match-syntax-pattern pattern form literals)
  (define (inner pattern form in-ellipsis)
    (cond
     ((and (null? pattern) (null? form)) '())
     ((and (is-ellipsis? pattern) (null? form)) '())
     ((member pattern literals)
      (if (eq? pattern form) '() #f))
     ((symbol? pattern) (list (cons pattern (cons in-ellipsis form))))
     ((and (pair? pattern) (not (pair? form))) #f)
     ((pair? pattern)
      (merge-matches (inner (car pattern) (car form)
                            (or in-ellipsis (is-ellipsis? pattern)))
                     (inner (if (is-ellipsis? pattern) pattern (cdr pattern))
                            (cdr form) in-ellipsis)))
     ((equal? pattern form) '())
     (else #f)))
  (inner pattern form #f))

;;(define-record-type no-match-type
;;  (no-match)
;;  no-match?)

(define (single-match name matches)
  (define match (assoc name matches))
  (cond
   ((not match) #f)
   ((car match)
    (error "use of ellipsis match in non-elipssis context" name))
   (else
    (cons (car match) (cddr match)))))

(define (nth-match name matches nth-rep)
  (cond
   ((null? matches) #f)
   ((not (eq? (caar matches) name))
    (nth-match name (cdr matches) nth-rep))
   ((not (cadar matches))
    (cons (caar matches) (cddar matches)))
   ((> nth-rep 0)
    (nth-match name (cdr matches) (- nth-rep 1)))
   (else
    (cons (caar matches) (cddar matches)))))

; gets the nth-match if nth-rep is truthy
(define (general-match name matches nth-rep)
  (if nth-rep
      (nth-match name matches nth-rep)
      (single-match name matches)))

(define (apply-ellipsis-pattern matches form nth-rep)
  )

(define (apply-syntax-pattern matches form nth-rep cont)
  (cond
   ((null? form) '())
   ((symbol? form)
    (general-match form matches nth-rep) form)
   ((is-ellipsis? form)
    (append (apply-ellipsis-pattern matches (car






;    ))
