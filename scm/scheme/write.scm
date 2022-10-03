(define-library (scheme write)
  (import (scheme base)
          (pscheme record)
          (pscheme options))
  (export write
          display)
  (begin

    (define (integer-write n port)
      (cond
       ((negative? n)
        (write-char #\- port)
        (integer-write (- n) port))
       ((> n 0)
        (integer-write (quotient n 10) port)
        (write-char (integer->char (+ (remainder n 10) (char->integer #\0))) port))))

    (define (list-write form port display)
      (unified-write (car form) port display)
      (cond
       ((pair? (cdr form))
        (write-char #\space port)
        (list-write (cdr form) port display))
       ((null? (cdr form)))
       (else
        (write-string " . " port)
        (unified-write (cdr form) port display))))

    ;; TODO: sanitize strings

    (define (string-write form port display)
      (unless display
        (write-char #\" port))
      (write-string form port)
      (unless display
        (write-char #\" port)))

    (define (char-write form port display)
      (if display
          (write-char form port)
          (begin
            (write-string "#\\" port)
            (case form
              ((#\alarm)     (write-string "alarm" port))
              ((#\backspace) (write-string "backspace" port))
              ((#\delete)    (write-string "delete" port))
              ((#\escape)    (write-string "escape" port))
              ((#\newline)   (write-string "newline" port))
              ((#\null)      (write-string "null" port))
              ((#\return)    (write-string "return" port))
              ((#\space)     (write-string "space" port))
              ((#\tab)       (write-string "tab" port))
              (else (write-char form port))))))

    ;; TODO: use parameters
    (define (unified-write form port display)
      (cond
       ((zero? form) (write-char #\0 port))
       ((integer? form) (integer-write form port))
       ((pair? form)
        (write-char #\( port)
        (list-write form port display)
        (write-char #\) port))
       ((vector? form)
        (write-char #\# port)
        (unified-write (vector->list form) port display))
       ((record? form)
        (write-string "#<" port)
        (write-string (symbol->string (record-symbol form)) port)
        (write-string ">" port))
       ((char? form) (char-write form port display))
       ((string? form) (string-write form port display))
       ((symbol? form) (write-string (symbol->string form) port))
       ((null? form) (write-string "()" port))
       ((eq? form '#f) (write-string "#f" port))
       ((eq? form '#t) (write-string "#t" port))
       ((eq? form (begin)) (write-string "#<unspecified>" port))
       (else (write-string "#<?>" port))))

    (define (write form . rest)
      (options rest (port (current-output-port)))
      (unified-write form port #f))

    (define (display form . rest)
      (options rest (port (current-output-port)))
      (unified-write form port #t))

    ))
