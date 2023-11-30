(import (util))

(define (random-ascii-char)
  (let ((ch (+ 32 (random 96))))
    (integer->char ch)))

(define (generate-string n)
  (define str (make-string n))
  (map 
    (lambda (i) (string-set! str i (random-ascii-char)))
    (iota n))
  str)

(define (needle str n)
  (define end (string-length str))
  (substring str (- end n) end))

(define (find-test finder n m)
  (define str (generate-string n))
  (define what (needle str m))
  (time (finder str what)))

(define str (make-string 20000 #\a))
(string-set! str 19999 #\b)
(time (string-find str "b"))
(time (string-find str #\b))