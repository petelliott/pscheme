(define-library (pscheme compiler file)
  (import (scheme base)
          (scheme char))
  (export make-span
          span?
          span-form
          span-file
          span-sr
          span-sc
          span-er
          span-ec
          span-read-all
          normal-read
          strip-spans)
  (begin

    (define-record-type span
      (make-span form file sr sc er ec)
      span?
      (form span-form)
      (file span-file)
      (sr span-sr)
      (sc span-sc)
      (er span-er)
      (ec span-ec))

    (define spans-on (make-parameter #f))
    (define file (make-parameter #f))
    (define row (make-parameter 0))
    (define col (make-parameter 0))

    (define (get-char port)
      (define ch (read-char port))
      (if (eqv? ch #\newline)
          (begin
            (col 0)
            (row (+ (row) 1)))
          (col (+ (col) 1)))
      ch)

    (define-syntax with-span
      (syntax-rules ()
        ((_ forms ...)
         (let* ((sr (row))
                (sc (col))
                (result (begin forms ...)))
           (if (spans-on)
               (make-span result (file) sr sc (row) (col))
               result)))))

    (define (no-eof port)
      (if (eof-object? (peek-char port))
          (error "unexpected eof" (file) (row) (col))))

    (define (skip-line port)
      (define ch (peek-char port))
      (if (not (or (eqv? ch #\newline) (eof-object? ch)))
          (get-char port)))

    (define (skip-cws port)
      (define pch (peek-char port))
      (cond
       ((eof-object? pch) (begin))
       ((char-whitespace? pch)
        (get-char port)
        (skip-cws port))
       ((char=? pch #\;)
        (skip-line port)
        (skip-cws port))))

    (define (cread-list port)
      (skip-cws port)
      (no-eof port)
      (case (peek-char port)
        ((#\))
         (get-char port)
         '())
        ((#\.)
         (get-char port)
         (let ((form (cread-any port)))
           (skip-cws port)
           (if (not (eqv? (get-char port) #\)))
               (error "expected ')'" (file) (row) (col)))
           form))
        (else
         (cons (cread-any port)
               (cread-list port)))))

    (define (cread-string port)
      (define (inner)
        (define ch (begin
                     (no-eof port)
                     (get-char port)))
        (case ch
          ((#\") '())
          ((#\\)
           (cons
            (case (get-char port)
              ((#\a) #\alarm)
              ((#\b) #\backspace)
              ((#\t) #\tab)
              ((#\n) #\newline)
              ((#\r) #\return)
              ((#\") #\")
              ((#\\) #\\)
              ((#\|) #\|)
              ((#\newline) #\newline)
              ((#\e) #\space)) ;; non-standard extension
            (inner)))
          (else
           (cons ch (inner)))))
      (list->string (inner)))

    (define (symbol-char? ch)
      (or (char-alphabetic? ch)
          (char-numeric? ch)
          (member ch '(#\! #\$ #\& #\* #\+ #\- #\. #\/ #\: #\< #\= #\> #\? #\@ #\^ #\_ #\-))))

    (define (cread-seq port)
      (if (symbol-char? (peek-char port))
          (cons (get-char port) (cread-seq port))
          '()))

    (define (num-lit-seq? seq)
      (or (null? seq)
          (and (or (char-numeric? (car seq))
                   (member (car seq) '(#\. #\- #\/)))
               (num-lit-seq? (cdr seq)))))

    (define (cread-hash-sequence port)
      (cond
       ((char=? #\( (peek-char port))
        (list->vector (strip-spans (cread-any port))))
       ((char=? #\\ (peek-char port))
        (get-char port)
        (car (cread-seq port))) ;; TODO: add #\newline and stuff
       (else
        (let ((str (list->string (cread-seq port))))
          (cond
           ((equal? "f" str) #f)
           ((equal? "t" str) #t))))))

    (define (cread-symbol-or-number port)
      (define seq (cread-seq port))
      (if (num-lit-seq? seq)
          (string->number (list->string seq))
          (string->symbol (list->string seq))))

    (define (cread-any port)
      (skip-cws port)
      (if (eof-object? (peek-char port))
          (get-char port)
          (with-span
           (case (peek-char port)
             ((#\()
              (get-char port)
              (cread-list port))
             ((#\")
              (get-char port)
              (cread-string port))
             ((#\')
              (get-char port)
              (cons quote (cread-any port)))
             ((#\#)
              (get-char port)
              (cread-hash-sequence port))
             (else
              (cread-symbol-or-number port))))))

    (define (span-read-all port filename)
      (define (inner)
        (define obj (cread-any port))
        (if (eof-object? obj)
            '()
            (cons obj (inner))))
      (parameterize ((file filename)
                     (row 0)
                     (col 0)
                     (spans-on #t))
        (inner)))

    (define (normal-read port)
      (parameterize ((spans-on #f))
        (cread-any port)))

    (define (strip-spans form)
      (cond
       ((span? form)
        (strip-spans (span-form form)))
       ((pair? form)
        (cons (strip-spans (car form))
              (strip-spans (cdr form))))
       (else form)))

    ))
