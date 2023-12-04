(import (util))

(define data (read-lines-all "./data/input4.txt"))

(define (parse-line str)
  (define (get-numbers s)
    (map string->number (filter string-non-empty? (split s " "))))
  (let* ((prefix (split str ":"))
         (id (string->number (car (reverse (split (car prefix) " ")))))
         (parts (map get-numbers (split (cadr prefix) "|"))))
    (cons id parts)))

(define (matching card)
  (define on-hand (cadr card))
  (define winners (caddr card))
  (define (win? c) (memq c winners))
  (length (filter win? on-hand)))

(define (score card)
  (let ((m (matching card)))
    (if (> m 0)
        (expt 2 (- m 1))
        0)))

(define example 
  '("Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"
    "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19"
    "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1"
    "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83"
    "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36"
    "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"))

(define (solve1 data)
  (apply + (map score (map parse-line data))))

(assert (= (solve1 example) 13))

(printf "~a\n" (solve1 data))

(define (solve2 data)
  (define rounds (map parse-line data))
  (define cards (make-eq-hashtable))
  (define (add-card c n)
    (hashtable-update! cards c (lambda (x) (+ x n)) 0))
  (for-each (lambda (c) (add-card c 1)) (map car rounds))
  (let loop ((r rounds) (total 0))
    (let* ((current (car r))
           (rest (cdr r))
           (id (car current)) 
           (n (hashtable-ref cards id 0)))
      (if (null? rest)
          (+ total n)
          (let ((m (matching current)))
            (for-each (lambda (x) (add-card (+ x id 1) n)) (iota m))
            (loop rest (+ total n)))))))

(assert (= (solve2 example) 30))

(printf "~a\n" (solve2 data))