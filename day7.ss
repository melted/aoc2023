(import (util))

(define data
  (map (lambda (l) (split-trim l " ")) (read-lines-all "./data/input7.txt")))

(define (parse-card c)
  (case c
    ((#\A) 14)
    ((#\K) 13)
    ((#\Q) 12)
    ((#\J) 11)
    ((#\T) 10)
    (else (char- c #\0))))

(define-record-type hand
  (fields cards value)
  (nongenerative))

(define (parse-line line)
  (make-hand (map parse-card (string->list (car line)))
             (string->number (cadr line))))

(define (parse-data data)
  (map parse-line data))

(define (count hand)
  (define histogram (make-eq-hashtable))
  (for-each (lambda (c) (hashtable-update! histogram c (lambda (x) (+ x 1)) 0))
            (hand-cards hand))
  (let ((jokers (hashtable-ref histogram 1 0)))
    (hashtable-delete! histogram 1)
    (let-values (((keys vals) (hashtable-entries histogram)))
      (let ((sorted (list-sort > (vector->list vals))))
        (if (null? sorted)
            (list jokers)
            (cons (+ (car sorted) jokers) (cdr sorted)))))))

(define (hand-type hand)
  (define dist (count hand))
  (case (car dist)
    ((5) 7)
    ((4) 6)
    ((3) (if (= (cadr dist) 2) 5 4))
    ((2) (if (= (cadr dist) 2) 3 2))
    (else 1)))

(define (tiebreak h1 h2)
  (define diffs (filter (lambda (x) (not (zero? x))) 
                        (map - (hand-cards h1) (hand-cards h2))))
  (negative? (car diffs)))

(define (hand<? h1 h2)
  (let ((t1 (hand-type h1))
        (t2 (hand-type h2)))
    (if (= t1 t2)
        (tiebreak h1 h2)
        (< t1 t2))))

(define (solve data)
  (define hands (list-sort hand<? (parse-data data)))
  (fold-left + 0 (map (lambda (h i) (* (hand-value h) (+ i 1))) hands (enumerate hands))))

(define example '(("32T3K" "765")
                  ("T55J5" "684")
                  ("KK677" "28")
                  ("KTJJT" "220")
                  ("QQQJA" "483")))

(assert (= (solve example) 6440))

(printf "~a\n" (solve data))

(define (jokerify hands)
  (define (jokerify-hand h)
     (make-hand (map (lambda (c) (if (= c 11) 1 c)) (hand-cards h)) 
                (hand-value h)))
  (map jokerify-hand hands))

(define (solve2 data)
  (define hands (list-sort hand<? (jokerify (parse-data data))))
  (fold-left + 0 (map (lambda (h i) (* (hand-value h) (+ i 1))) hands (enumerate hands))))

(assert (= (solve2 example) 5905))

(printf "~a\n" (solve2 data))
