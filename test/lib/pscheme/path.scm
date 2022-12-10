(import (scheme base)
        (pscheme path)
        (pscheme test))

(define-test "path-is-absolute?"
  (assert (path-is-absolute? "/hello/world"))
  (assert (path-is-absolute? "/"))
  (assert (not (path-is-absolute? "hello/world")))
  (assert (not (path-is-absolute? ""))))

(define-test "dirname"
  (assert (equal? (dirname "/usr/lib") "/usr"))
  (assert (equal? (dirname "/usr/") "/"))
  (assert (equal? (dirname "usr") "."))
  (assert (equal? (dirname "/") "/"))
  (assert (equal? (dirname ".") "."))
  (assert (equal? (dirname "..") "."))
  (assert (equal? (dirname "") ".")))

(define-test "basename"
  (assert (equal? (basename "/usr/lib") "lib"))
  (assert (equal? (basename "/usr/") "usr"))
  (assert (equal? (basename "usr") "usr"))
  (assert (equal? (basename "/") "/"))
  (assert (equal? (basename ".") "."))
  (assert (equal? (basename "..") ".."))
  (assert (equal? (basename "") ".")))

(finish-tests)
