(define-library (pscheme string-builder)
  (import (scheme base)
          (scheme write))
  (export string-builder?
          make-string-builder
          string-builder-append
          string-builder-build)
  (begin

    (define-record-type string-builder
      (construct-string-builder buff len cap)
      string-builder?
      (buff sb-buff sb-set-buff!)
      (len sb-len sb-set-len!)
      (cap sb-cap sb-set-cap!))

    (define (make-string-builder)
      ;; 15 so it fits in a cell
      (construct-string-builder (make-string 15) 0 15))

    (define (make-space builder obj-size)
      (unless (>= (sb-cap builder) (+ (sb-len builder) obj-size))
        (let* ((newcap (+ obj-size (* (sb-cap builder) 2)))
               (newstr (make-string newcap)))
          (string-copy! newstr 0 (sb-buff builder))
          (sb-set-buff! builder newstr)
          (sb-set-cap! builder newcap))))

    (define (string-builder-append builder obj)
      (if (char? obj)
          (begin
            (make-space builder 1)
            (string-set! (sb-buff builder) (sb-len builder) obj)
            (sb-set-len! builder (+ 1 (sb-len builder))))
          (let ((len (string-length obj)))
            (make-space builder len)
            (string-copy! (sb-buff builder) (sb-len builder) obj)
            (sb-set-len! builder (+ len (sb-len builder))))))

    (define (string-builder-build builder)
      (string-copy (sb-buff builder) 0 (sb-len builder)))

    ))
