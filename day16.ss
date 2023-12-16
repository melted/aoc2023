(import (util))

(define data (list->vector (read-lines-all "./data/input16.txt")))

(define (width data) (string-length (vector-ref data 0)))
(define (height data) (vector-length data))

(define (x pos) (vector-ref pos 0))
(define (y pos) (vector-ref pos 1))

(define (in-bounds? data pos)
  (and (< -1 (x pos) (width data)) (< -1 (y pos) (height data))))

(define (at data pos)
  (if (in-bounds? data pos)
      (string-ref (vector-ref data (y pos)) (x pos))
      #\#))

(define (delta dir)
  (case dir
    ((north) '#(0 -1))
    ((south) '#(0 1))
    ((east) '#(1 0))
    ((west) '#(-1 0))))

(define (pos+ p1 p2)
  (vector (+ (x p1) (x p2))
          (+ (y p1) (y p2))))

(define (rayx x y dir)
  (list (vector x y) dir))

(define (ray pos dir)
  (list pos dir))

(define (interact ch direction pos)
  (case ch
    ((#\|) (if (or (eq? direction 'north) (eq? direction 'south))
               (list (ray pos direction))
               (list (ray pos 'north) (ray pos 'south))))
    ((#\-) (if (or (eq? direction 'east) (eq? direction 'west))
               (list (ray pos direction))
               (list (ray pos 'east) (ray pos 'west))))
    ((#\\) (list (ray pos 
                      (case direction
                        ((north) 'west)
                        ((south) 'east)
                        ((east) 'south)
                        ((west) 'north)))))
    ((#\/) (list (ray pos 
                      (case direction
                        ((north) 'east)
                        ((south) 'west)
                        ((east) 'north)
                        ((west) 'south)))))))

(define (trace data energized ray)
  (define start (car ray))
  (define direction (cadr ray))
  (let loop ((pos start))
    (when (in-bounds? data pos)
      (hashtable-update! energized pos add1 0))
    (let ((next (pos+ pos (delta direction))))
      (if (in-bounds? data next)
          (if (char=? (at data next) #\.)
              (loop next)
              (interact (at data next) direction next))
          '()))))

(define (ray-trace data start)
  (define traced (make-hashtable equal-hash equal?))
  (define energized (make-hashtable equal-hash equal?))
  (let loop ((todo (list start)))
    (if (null? todo)
        (hashtable-size energized)
        (let ((next (car todo)))
          (if (hashtable-contains? traced next)
              (loop (cdr todo))
              (let ((new (trace data energized next)))
                (hashtable-set! traced next #f)
                (loop (append new (cdr todo)))))))))

(define (solve data)
  (define start (ray (vector -1 0) 'east))
  (ray-trace data start))

(define example '#(
  ".|...\\...."
  "|.-.\\....."
  ".....|-..."
  "........|."
  ".........."
  ".........\\"
  "..../.\\\\.."
  ".-.-/..|.."
  ".|....-|.\\"
  "..//.|...."))

(define (show data)
  (for-each (lambda (y) (printf "~a\n" (vector-ref data y))) (iota (height data))))

(assert (= (solve example) 46))

(printf "~a\n" (solve data))
(define (solve2 data)
  (define starts (append (map (lambda (x) (rayx x -1 'south)) (iota (width data)))
                         (map (lambda (x) (rayx x (height data) 'north)) (iota (width data)))
                         (map (lambda (y) (rayx -1 y 'east)) (iota (height data)))
                         (map (lambda (y) (rayx (width data) y 'west)) (iota (height data)))))
  (apply max (map (lambda (s) (ray-trace data s)) starts)))

(assert (= (solve2 example) 51))

(printf "~a\n" (solve2 data))