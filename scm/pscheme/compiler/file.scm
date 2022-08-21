(define-library (pscheme compiler file)
  (import (scheme base)
          (scheme char)
          (pscheme compiler error))
  (export make-span
          span?
          span-form
          span-file
          span-sr
          span-sc
          span-er
          span-ec
          copy-span
          span-read-all
          normal-read
          unspan1
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

    (define (copy-span span new-form)
      (make-span
       new-form
       (span-file span)
       (span-sr span)
       (span-sc span)
       (span-er span)
       (span-ec span)))

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
          (pscm-err (with-span #f) "read: unexpected ~a" (peek-char port))))

    (define (skip-line port)
      (define ch (peek-char port))
      (if (not (or (eqv? ch #\newline) (eof-object? ch)))
          (begin
            (get-char port)
            (skip-line port))))

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
         (let ((seq (cread-seq port)))
           (if (= (length seq) 1)
               (begin
                 (get-char port)
                 (let ((form (cread-any port)))
                   (skip-cws port)
                   (let ((ch (get-char port)))
                     (if (not (eqv? ch #\)))
                         (pscm-err (with-span #f) "read: expected ~a, got ~a" #\) ch)))
                   form))
               (cons (seq->symbol-or-number seq)
                     (cread-list port)))))
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

    (define (named-character n span)
      (cond
       ((equal? n "alarm") #\alarm)
       ((equal? n "backspace") #\backspace)
       ((equal? n "delete") #\delete)
       ((equal? n "escape") #\escape)
       ((equal? n "newline") #\newline)
       ((equal? n "null") #\null)
       ((equal? n "return") #\return)
       ((equal? n "space") #\space)
       ((equal? n "tab") #\tab)
       (else (pscm-err span "read: unknown character name ~a" n))))

    (define (cread-hash-sequence port)
      (cond
       ((char=? #\( (peek-char port))
        (list->vector (strip-spans (cread-any port))))
       ((char=? #\\ (peek-char port))
        (get-char port)
        (if (symbol-char? (peek-char port))
            (let ((seq (with-span (cread-seq port))))
              (if (null? (cdr (span-form seq)))
                  (car (span-form seq))
                  (named-character (list->string (span-form seq)) seq)))
            (get-char port)))
       (else
        (let ((str (list->string (cread-seq port))))
          (cond
           ((equal? "f" str) #f)
           ((equal? "t" str) #t))))))

    (define (seq->symbol-or-number seq)
      (define str (list->string seq))
      (or (string->number str)
          (string->symbol str)))

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
              `(quote ,(cread-any port)))
             ((#\#)
              (get-char port)
              (cread-hash-sequence port))
             (else
              (let ((seq (cread-seq port)))
                (if (null? seq)
                    (pscm-err (with-span #f) "read: unexpected character ~a" (peek-char port))
                    (seq->symbol-or-number seq))))))))

    (define (span-read-all port filename)
      (define (inner)
        (define obj (cread-any port))
        (if (eof-object? obj)
            '()
            (cons obj (inner))))
      (parameterize ((file filename)
                     (row 1)
                     (col 0)
                     (spans-on #t))
        (inner)))

    (define (normal-read port)
      (parameterize ((spans-on #f))
        (cread-any port)))

    (define (unspan1 form)
      (if (span? form)
          (span-form form)
          form))

    (define (strip-spans form)
      (cond
       ((span? form)
        (strip-spans (span-form form)))
       ((pair? form)
        (cons (strip-spans (car form))
              (strip-spans (cdr form))))
       (else form)))

    ))
