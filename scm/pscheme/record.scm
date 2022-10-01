(define-library (pscheme record)
  (export record?
          record-symbol)
  (begin

    (define (record? obj)
      (builtin slots? obj))

    (define (record-symbol obj)
      (builtin slot-ref obj 0))

    ))
