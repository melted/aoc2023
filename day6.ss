(import (util))

(define data '((42 . 284)
               (68 . 1005)
               (69 . 1122)
               (85 . 1341)))

(define example '((7 . 9) (15 . 40) (30 . 200)))

(define (treshold t d)
  (/ (- t (sqrt (- (* t t) (* 4 d)))) 2))

(define (winners t d)
  (let* ((tr (ceiling (treshold t d)))
         (win (if (= (* tr (- t tr)) d) (+ tr 1) tr)))
    (+ (- t (* 2 win)) 1)))

(define (solve data)
  (fold-left * 1 (map (lambda (p) (winners (car p) (cdr p))) data)))

(assert (= (solve example) 288))

(printf "~a\n" (solve data))

(assert (= (winners 71530 940200) 71503))

(printf "~a\n" (winners 42686985 284100511221341))