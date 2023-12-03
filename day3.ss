(import (util))

(define data
  (list->vector
    (read-lines-all "d:/Niklas/repos/aoc2023/data/input3.txt")))

(define current-data (make-parameter data))
(define (width) (string-length (vector-ref (current-data) 0)))
(define (height) (vector-length (current-data)))

(define (in-bounds? x y)
  (and (<= 0 x (- (width) 1)) (<= 0 y (- (height) 1))))

(define (get-at x y)
  (if (in-bounds? x y)
    (string-ref (vector-ref (current-data) y) x)
    #\.))

(define (neighbors x1 x2 y)
  (let loop ((cx (- x1 1)) (cy (- y 1)) (found '()))
    (let ((ch (get-at cx cy)))
      (cond
        ((> cy (+ y 1)) found)
        ((> cx (+ x2 1)) (loop (- x1 1) (+ cy 1) found))
        ((or (char-numeric? ch) (char=? ch #\.))
              (loop (+ cx 1) cy found))
        (else (loop (+ cx 1) cy (cons ch found)))))))

(define (neighbors? x1 x2 y)
  (not (null? (neighbors x1 x2 y))))

(define (find-nums-and-gears str y)
  (define end (string-length str))
  (let loop ((x 0) (in-num #f) (acc '()) (gears '()))
    (let ((ch (if (< x end) (string-ref str x) #\.)))
      (cond
        ((and (= x end) in-num) (cons gears (cons (list (string->number (substring str in-num x)) in-num (- x 1) y) acc)))
        ((= x end) (cons gears acc))
        ((and (char-numeric? ch) (not in-num)) (loop (+ x 1) x acc gears))
        ((and (not (char-numeric? ch)) in-num)
            (loop (+ x 1) #f (cons (list (string->number (substring str in-num x)) in-num (- x 1) y) acc) (if (char=? ch #\*) (cons (cons x y) gears) gears)))
        ((char=? ch #\*) (loop (+ x 1) #f acc (cons (cons x y) gears)))
        (else (loop (+ x 1) in-num acc gears))))))

(define (all-nums)
  (define ls (vector->list (current-data)))
  (define nums-and-gears (map (lambda (s y) (find-nums-and-gears s y)) ls (enumerate ls)))
  (cons (apply append (map car nums-and-gears)) (apply append (map cdr nums-and-gears))))

(define (solve1)
  (apply + (map car (filter (lambda (ls) (apply neighbors? (cdr ls))) (cdr (all-nums))))))


(define example
           '#("467..114.."
              "...*......"
              "..35..633."
              "......#..."
              "617*......"
              ".....+.58."
              "..592....."
              "......755."
              "...$.*...."
              ".664.598.."))

(assert (= (parameterize ((current-data example)) (solve1)) 4361))

(printf "~a\n" (solve1))

(define (in-range? number star)
  (and (<= (- (car number) 1) (car star) (+ (cadr number) 1))
       (<= (- (caddr number) 1) (cdr star) (+ (caddr number) 1))))

(define (get-pairs) 
  (define gears-and-nums (all-nums))
  (define (close gear) (map car (filter (lambda (num) (in-range? (cdr num) gear)) (cdr gears-and-nums))))
  (filter (lambda (n) (= (length n) 2)) (map close (car gears-and-nums))))

(define (solve2)
  (apply + (map (lambda (l) (apply * l)) (get-pairs))))

(assert (= (parameterize ((current-data example)) (solve2)) 467835))

(printf "~a\n" (solve2))