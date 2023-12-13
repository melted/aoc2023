(import (util))

(define data
  (read-lines-all "./data/input13.txt"))

(define (parse-patterns data)
  (define (accum a x)
    (if (null? a)
        (list (list x))
        (if (string=? "" x)
          (cons '() a)
          (cons (cons x (car a)) (cdr a)))))
  (map list->vector (map reverse  (fold-left accum '() data))))

(define (width pattern) (string-length (vector-ref pattern 0)))
(define (height pattern) (vector-length pattern)) 

(define (vertical pattern x)
  (list->string (map (lambda (y) (string-ref (vector-ref pattern y) x)) (iota (vector-length pattern)))))

(define (count-faults str1 str2)
  (length (filter (lambda (x) (not (char=? (car x) (cdr x)))) (map cons (string->list str1) (string->list str2)))))

(define (horizontal-reflect-at? pattern y faults)
  (define n (min y (- (height pattern) y)))
  (= (apply + (map (lambda (offset) (count-faults (vector-ref pattern (- y offset 1)) 
                                                  (vector-ref pattern (+ y offset))))
                    (iota n)))
      faults))

(define (vertical-reflect-at? pattern x faults)
  (define n (min x (- (width pattern) x)))
  (= (apply + (map (lambda (offset) (count-faults (vertical pattern (- x offset 1)) 
                                                  (vertical pattern (+ x offset)))) 
                   (iota n)))
      faults))

(define (reflects pattern faults)
  (define vr (exists (lambda (x) (and (vertical-reflect-at? pattern x faults) x)) (map add1 (iota (- (width pattern) 1)))))
  (define hr (exists (lambda (y) (and (horizontal-reflect-at? pattern y faults) y)) (map add1 (iota (- (height pattern) 1)))))
  (+ (if vr vr 0) (if hr (* 100 hr) 0)))

(define example '(
"#.##..##."
"..#.##.#."
"##......#"
"##......#"
"..#.##.#."
"..##..##."
"#.#.##.#."
""
"#...##..#"
"#....#..#"
"..##..###"
"#####.##."
"#####.##."
"..##..###"
"#....#..#"
))

(define (solve data faults)
  (apply + (map (lambda (x) (reflects x faults)) (parse-patterns data))))

(assert (= (solve example 0) 405))

(printf "~a\n" (solve data 0))

(assert (= (solve example 1) 400))

(printf "~a\n" (solve data 1))