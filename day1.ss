(import (util))

(define data
    (read-lines-all "./data/input1.txt"))

(define (num-list str)
  (define (char->digit chr) (char- chr #\0))
  (map char->digit (filter char-numeric? (string->list str))))

(define (value ls)
  (+ (* 10 (car ls)) (car (reverse ls))))

(define (solve1 d)
  (fold-left + 0 (map value (map num-list d))))

(assert (= (solve1 '("1abc2" "pqr3stu8vwx" "a1b2c3d4e5f" "treb7uchet")) 142))

(display (format "~a\n" (solve1 data)))

(define alphanums '(("one" . 1) ("two" . 2) ("three" . 3) 
                    ("four" . 4) ("five" . 5) ("six" . 6)
                    ("seven" . 7) ("eight" . 8) ("nine" . 9)))

(define (num-list2 str)
  (let loop ((nums '()) (i 0))
    (cond
      ((= i (string-length str)) (reverse nums))
      ((assp (lambda (s) (is-substring-at? str s i)) alphanums)
         => (lambda (p) (loop (cons (cdr p) nums) (+ i 1))))
      ((char-numeric? (string-ref str i)) 
          (loop (cons (char- (string-ref str i) #\0) nums) (+ i 1)))
      (else (loop nums (+ i 1))))))

(define (solve2 d)
  (fold-left + 0 (map value (map num-list2 d))))

(assert (= (solve2 '("two1nine"
                    "eightwothree"
                    "abcone2threexyz"
                    "xtwone3four"
                    "4nineeightseven2"
                    "zoneight234"
                    "7pqrstsixteen")) 281))

(display (format "~a\n" (solve2 data)))