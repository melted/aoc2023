(import (util))

(define data (read-lines-all "./data/input18.txt"))

(define (pos x y) (vector x y))
(define (dpos x y) (vector x y))
(define (x pos) (vector-ref pos 0))
(define (y pos) (vector-ref pos 1))

(define (pos+ p1 p2)
  (vector (+ (x p1) (x p2))
          (+ (y p1) (y p2))))

(define (addp p)
  (lambda (dp) (vector (+ (x p) (x dp))
          (+ (y p) (y dp)))))

(define (pos* n p)
  (vector (* n (x p)) (* n (y p))))

(define (pos= p1 p2)
  (and (= (x p1) (x p2)) (= (y p1) (y p2))))

(define (pos< p1 p2)
  (or (< (y p1) (y p2))
      (and (= (y p1) (y p2))
           (< (x p1) (x p2)))))


(define (parse-line1 str)
  (define parts (split-trim str " "))
  (list (car parts) (string->number (cadr parts))))

(define (parse-line2 str)
  (define parts (split-trim str " "))
  (translate (substring (caddr parts) 2 8)))

(define (translate hex)
  (define num (string->number (substring hex 0 5) 16))
  (define dir
   (case (string-ref hex 5)
    ((#\0) "R")
    ((#\1) "D")
    ((#\2) "L")
    ((#\3) "U")))
  (list dir num))

(define (delta cmd)
  (case cmd
    (("U") '#(0 -1))
    (("D") '#(0 1))
    (("R") '#(1 0))
    (("L") '#(-1 0))))

(define (add-connection-dir nodes p1 p2)
  (hashtable-update! nodes p1 (lambda (v) (vector-set! v (if (= (x p1) (x p2)) 0 1) p2) v) (vector #f #f)))

(define (add-connection nodes p1 p2)
  (add-connection-dir nodes p1 p2)
  (add-connection-dir nodes p2 p1))

(define (execute cmds)
  (define start (pos 0 0))
  (let loop ((c cmds) (p start) (acc '()) (dist 0))
    (if (null? c)
        (cons dist (reverse acc))
        (let ((new (pos+ p (pos* (cadr (car c)) (delta (caar c))))))
          (loop (cdr c) new (cons (cons p new) acc) (+ dist (cadr (car c))))))))

(define (calculate nodes dist)
  (define (accum t edge)
    (let* ((p1 (car edge))
          (p2 (cdr edge))
          (s (- (* (x p1) (y p2)) (* (x p2) (y p1)))))
      (+ t s)))
  (+ (div (+ dist (fold-left accum 0 nodes)) 2)1 ))

(define example '(
  "R 6 (#70c710)"
  "D 5 (#0dc571)"
  "L 2 (#5713f0)"
  "D 2 (#d2c081)"
  "R 2 (#59c680)"
  "D 2 (#411b91)"
  "L 5 (#8ceee2)"
  "U 2 (#caa173)"
  "L 1 (#1b58a2)"
  "U 2 (#caa171)"
  "R 2 (#7807d2)"
  "U 3 (#a77fa3)"
  "L 2 (#015232)"
  "U 2 (#7a21e3)"
))

(define (solve data parser)
  (define perimeter (execute (map parser data)))
  (calculate (cdr perimeter) (car perimeter)))

(assert (= (solve example parse-line1) 62))

(printf "~a\n" (solve data parse-line1))

(assert (= (solve example parse-line2) 952408144115))

(printf "~a\n" (solve data parse-line2))