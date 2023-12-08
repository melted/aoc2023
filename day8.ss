(import (util))

(define data (read-lines-all "./data/input8.txt"))

(define (parse-line str)
  (let* ((top (split-trim str "="))
         (r1 (cadr top))
         (r2 (substring r1 1 (- (string-length r1) 1)))
         (alts (split-trim r2 ",")))
    (cons (car top) alts)))

(define (parse-data data)
  (define ht (make-hashtable string-hash string=?))
  (define (add-node nod) (hashtable-set! ht (car nod) (cdr nod)))
  (for-each add-node (map parse-line (cddr data)))
  (cons (string->list (car data)) ht))

(define (follow turns ht start end?)
  (let loop ((n 0) (t turns) (c start))
    (cond
      ((end? c) n)
      ((null? t) (loop n turns c))
      (else
        (let ((next (hashtable-ref ht c #f)))
          (if next
              (loop (+ n 1) (cdr t) (if (char=? (car t) #\L) (car next) (cadr next)))
              (error 'follow "No way!")))))))

(define example '(
  "RL" ""

"AAA = (BBB, CCC)"
"BBB = (DDD, EEE)"
"CCC = (ZZZ, GGG)"
"DDD = (DDD, DDD)"
"EEE = (EEE, EEE)"
"GGG = (GGG, GGG)"
"ZZZ = (ZZZ, ZZZ)"
))

(define example2 '(
  "LLR" ""

"AAA = (BBB, BBB)"
"BBB = (AAA, ZZZ)"
"ZZZ = (ZZZ, ZZZ)"
))

(define (solve1 data)
  (define d (parse-data data))
  (define (end1 x) (string=? x "ZZZ"))
  (follow (car d) (cdr d) "AAA" end1))

(assert (= (solve1 example) 2))
(assert (= (solve1 example2) 6))

(printf "~a\n" (solve1 data))

(define (solve2 data)
  (define d (parse-data data))
  (define (end2 c) (suffix? c "Z"))
  (define starts (filter (lambda (s) (suffix? s "A"))
                        (vector->list (hashtable-keys (cdr d)))))
  (apply lcm (map (lambda (s) (follow (car d) (cdr d) s end2)) starts)))

(define example3 '(
"LR" ""

"11A = (11B, XXX)"
"11B = (XXX, 11Z)"
"11Z = (11B, XXX)"
"22A = (22B, XXX)"
"22B = (22C, 22C)"
"22C = (22Z, 22Z)"
"22Z = (22B, 22B)"
"XXX = (XXX, XXX)"
))

(assert (= (solve2 example3) 6))

(printf "~a\n" (solve2 data))