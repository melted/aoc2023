(import (util))

(define data
  (list->vector (read-lines-all "./data/input11.txt")))

(define (width data) (string-length (vector-ref data 0)))
(define (height data) (vector-length data))

(define (in-bounds? data x y)
  (and (< -1 x (width data)) (< -1 y (height data))))

(define (at data x y)
  (if (in-bounds? data x y)
    (string-ref (vector-ref data y) x)
    #\.))

(define (parse-data data)
  (let loop ((x 0) (y 0) (out '()))
    (cond 
      ((= y (height data)) (reverse out))
      ((= x (width data)) (loop 0 (+ y 1) out))
      ((char=? (at data x y) #\#) (loop (+ x 1) y (cons (cons x y) out)))
      (else (loop (+ x 1) y out)))))

(define (empty-rows data)
  (define (empty? y) (not (string-find (vector-ref data y) #\#)))
  (filter empty? (iota (height data))))

(define (empty-cols data)
  (define (empty? x) (for-all (lambda (y) (not (char=? (at data x y) #\#))) (iota (height data))))
  (filter empty? (iota (width data))))

(define (expand expansion-factor data galaxies)
  (define rows (empty-rows data))
  (define cols (empty-cols data))
  (define (f ls x)
    (if (or (null? ls) (< x (car ls)))
        x
        (+ (- expansion-factor 1) (f (cdr ls) x))))
  (define (move g) 
    (let ((dx (f cols (car g)))
          (dy (f rows (cdr g))))
      (cons dx dy)))
  (map move galaxies))

(define (dist g1 g2)
  (+ (abs (- (car g1) (car g2))) (abs (- (cdr g1) (cdr g2)))))

(define (solve expansion-factor data)
  (define galaxies (parse-data data))
  (define expanded (expand expansion-factor data galaxies))
  (let loop ((g expanded) (sum 0))
    (if (null? g)
        sum
        (let ((s (apply + (map (lambda (x) (dist (car g) x)) (cdr g)))))
          (loop (cdr g) (+ s sum))))))

(define example '#(
"...#......"
".......#.."
"#........."
".........."
"......#..."
".#........"
".........#"
".........."
".......#.."
"#...#....."
))

(assert (= (solve 2 example) 374))

(printf "~a\n" (solve 2 data))

(assert (= (solve 100 example) 8410))

(printf "~a\n" (solve 1000000 data))