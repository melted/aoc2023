(import (util))

(define data (list->vector (read-lines-all "./data/input21.txt")))

(define (x pos) (vector-ref pos 0))
(define (y pos) (vector-ref pos 1))

(define (pos+ p1 p2)
  (vector (+ (x p1) (x p2))
          (+ (y p1) (y p2))))

(define (parse-data data)
  (define lines (vector->list data))
  (define obstacles (make-hashtable equal-hash equal?))
  (define start #f)
  (define (parse-line str y) 
    (for-each (lambda (ch x) 
                (case ch
                  ((#\#) (hashtable-set! obstacles (vector x y) #t))
                  ((#\S) (set! start (vector x y)))))
              (string->list str)
              (iota (string-length str))))
  (for-each parse-line lines (enumerate lines))
  (values obstacles start))

(define current-data (make-parameter data))

(define (width)
  (string-length (vector-ref (current-data) 0)))

(define (height)
  (vector-length (current-data)))

(define neighbors (list (vector 0 1) (vector 0 -1) (vector 1 0) (vector -1 0)))

(define (is-obstacle? obstacles) 
  (lambda (pos)
    (hashtable-contains? obstacles
                         (vector (mod (x pos) (width))
                                 (mod (y pos) (height))))))

(define (walk obstacles now)
  (let ((dests (map (lambda (p) (pos+ now p)) neighbors)))
    (remp (is-obstacle? obstacles) dests)))


(define (reach-steps obstacles start n)
  (define positions (make-hashtable equal-hash equal?))
  (let loop ((p (list start)) (t 0))
    (if (= t n)
        (length p)
        (begin
          (hashtable-clear! positions)
          (for-each (lambda (d) (hashtable-set! positions d #t))
                    (apply append (map (lambda (w) (walk obstacles w)) p)))
          (loop (vector->list (hashtable-keys positions)) (+ t 1))))))

(define (solve data n)
  (define-values (obstacles start) (parse-data data))
  (reach-steps obstacles start n))

(define example '(
  "..........."
  ".....###.#."
  ".###.##..#."
  "..#.#...#.."
  "....#.#...."
  ".##..S####."
  ".##..#...#."
  ".......##.."
  ".##.#.####."
  ".##..##.##."
  "..........."
))

(define (solve2 data)
  (define-values (obstacles start) (parse-data data))
  (define a (solve data 65))
  (define b (solve data 196))
  (define c (solve data 327))
  (define n 26501365)
  (define i (/ (- n 65) 131))
  (+ a (* i (- b a)) (* (/ (* i (- i 1)) 2) (+ a c (* -2 b)))))