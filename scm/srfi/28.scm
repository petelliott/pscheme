(define-library (srfi 28)
  (import (scheme base)
          (scheme write))
  (export format)
  (begin

;;; this implementation of format is modified from the srfi-28 reference implementation
;;; https://srfi.schemers.org/srfi-28/srfi-28.html

;;; Copyright (C) Scott G. Miller (2002). All Rights Reserved.
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a copy
;;; of this software and associated documentation files (the "Software"), to deal in
;;; the Software without restriction, including without limitation the rights to
;;; use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
;;; the Software, and to permit persons to whom the Software is furnished to do so,
;;; subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in
;;; all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
;;; FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
;;; COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
;;; IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

    (define (format format-string . objects)
      (define buffer (open-output-string))
      (let loop ((format-list (string->list format-string))
                 (objects objects))
        (cond
         ((null? format-list) (get-output-string buffer))
         ((char=? (car format-list) #\~)
          (if (null? (cdr format-list))
              (error "format: Incomplete escape sequence")
              (case (cadr format-list)
                ((#\a)
                 (if (null? objects)
                     (error "format: No value for escape sequence")
                     (begin
                       (display (car objects) buffer)
                       (loop (cddr format-list) (cdr objects)))))
                ((#\s)
                 (if (null? objects)
                     (error "format: No value for escape sequence")
                     (begin
                       (write (car objects) buffer)
                       (loop (cddr format-list) (cdr objects)))))
                ((#\%)
                 (newline buffer)
                 (loop (cddr format-list) objects))
                ((#\~)
                 (write-char #\~ buffer)
                 (loop (cddr format-list) objects))
                (else
                 (error "format: Unrecognized escape sequence")))))
         (else (write-char (car format-list) buffer)
               (loop (cdr format-list) objects)))))

    ))
