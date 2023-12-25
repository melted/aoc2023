(import (util))

(define data (read-lines-all "./data/input24.txt"))

(define (parse-line str)
  (define (make-3d v) (list->vector (map string->number (split-trim v ","))))
  (define parts (map make-3d (split-trim str "@")))
  (list (car parts) (cadr parts)))