(import (util))

(define data
  (list->vector (read-lines-all "./data/input14.txt")))

(define example
  '#( "O....#...."
      "O.OO#....#"
      ".....##..."
      "OO.#O....O"
      ".O.....O#."
      "O.#..O.#.#"
      "..O..#O..O"
      ".......O.."
      "#....###.."
      "#OO..#...."))

(define (data-copy data)
  (define new (make-vector (vector-length data)))
  (for-each (lambda (i) (vector-set! new i (string-copy (vector-ref data i))))
            (iota (vector-length data)))
  new)

(define (width data) (string-length (vector-ref data 0)))
(define (height data) (vector-length data))

(define (in-bounds? data x y)
  (and (< -1 x (width data)) (< -1 y (height data))))

(define (at data x y)
  (if (in-bounds? data x y)
      (string-ref (vector-ref data y) x)
      #\#))

(define (write-at data x y ch)
  (when (in-bounds? data x y)
    (string-set! (vector-ref data y) x ch)))

(define (turn data)
  (define new-data (make-vector (width data)))
  (define (new-row y)
    (vector-set! new-data 
                 y
                 (list->string (reverse (map (lambda (x) (string-ref (vector-ref data x) y)) 
                       (iota (height data)))))))
  (for-each new-row (iota (width data)))
  new-data)

(define (drop-level data y)
  (define (drop x)
    (when (and (char=? (at data x y) #\O) (char=? (at data x (- y 1)) #\.))
      (write-at data x (- y 1) #\O)
      (write-at data x y #\.)))
  (when (> y 0)
    (for-each drop (iota (width data)))
    (drop-level data (- y 1))))

(define (drop-rocks data)
  (define d (data-copy data))
  (for-each (lambda (y) (drop-level d y)) (iota (height d)))
  d)

(define (show data)
  (for-each (lambda (y) (printf "~a\n" (vector-ref data y))) (iota (height data))))

(define (score data)
  (define (count str) 
    (length (filter (lambda (ch) (char=? ch #\O)) (string->list str))))
  (apply + (map (lambda (y) (* (- (height data) y) (count (vector-ref data y)))) (iota (height data)))))

(assert (= (score (drop-rocks example)) 136))

(printf "~a\n" (score (drop-rocks data)))

(define (cycle data)
  (do ((i 0 (+ i 1)) 
       (d data (turn (drop-rocks d))))
       ((= i 4) d)))

(define (solve2 data)
  (define states (make-hashtable equal-hash equal?))
  (define by-time (make-hashtable equal-hash equal?))
  (let loop ((t 0) (d data))
    (let ((previous (hashtable-ref states d #f)))
      (if previous
          (let* ((cl (- t previous))
                (final (+ (mod (- 1000000000 previous) cl) previous))
                (final-score (hashtable-ref by-time final #f)))
            final-score)
          (begin
            (hashtable-set! states d t)
            (hashtable-set! by-time t (score d))
            (loop (+ t 1) (cycle d)))))))

(assert (= (solve2 example) 64))

(printf "~a\n" (solve2 data))