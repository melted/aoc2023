(import (util))

(define data (read-lines-all "./data/input24.txt"))

(define (parse-line str)
  (define (make-3d v) (list->vector (map inexact (map string->number (split-trim v ",")))))
  (map make-3d (split-trim str "@")))

(define (v+ p v)
  (vector-map + p v))

(define (n* n v)
  (vector-map (lambda (x) (* n x)) v))

(define (x v)
  (vector-ref v 0))


(define (y v)
  (vector-ref v 1))

(define (z v)
  (vector-ref v 2))

(define (intersection-2d p1 v1 p2 v2)
  (let ((denom (- (* (x v2) (y v1)) (* (x v1) (y v2)))))
    (if (= denom 0)
        #f
        (let* ((t1 (/ (- (* (y v2) (- (x p1) (x p2)))
                         (* (x v2) (- (y p1) (y p2))))
                denom))
               (t2 (/ (+ (- (y p1) (y p2)) (* (y v1) t1)) (y v2))))
           (if (or (< t1 0.0) (< t2 0.0))
               #f
               (v+ p1 (n* t1 v1)))))))

(define (check a b range)
  (define (in-range? x) (< (car range) x (cadr range)))
  (define pos (intersection-2d (car a) (cadr a) (car b) (cadr b)))
  (and pos (in-range? (x pos)) (in-range? (y pos))))

(define (score points range)
  (if (null? (cdr points))
      0
      (let ((s (length (filter (lambda (x) (check (car points) x range)) (cdr points)))))
        (+ (score (cdr points) range) s))))

(define (solve data range)
  (define points (map parse-line data))
  (score points range))

(define example '(
  "19, 13, 30 @ -2,  1, -2"
  "18, 19, 22 @ -1, -1, -2"
  "20, 25, 34 @ -2, -2, -4"
  "12, 31, 28 @ -1, -2, -1"
  "20, 19, 15 @  1, -5, -3"
))
(assert (= (solve example (list 7 27)) 2))

(printf "~a\n" (solve data (list 200000000000000 400000000000000)))