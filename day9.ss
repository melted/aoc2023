(import (util))

(define data (read-lines-all "./data/input9.txt"))

(define (parse-line str)
  (map string->number (split-trim str " ")))

(define (diff lst)
  (map - (cdr lst) (reverse (cdr (reverse lst)))))

(define (next-value seq)
  (define diff-seqs
    (let loop ((s seq) (acc '()))
      (let ((d (diff s)))
        (cond
          ((for-all zero? d) (cons s acc))
          ((= (length d) 1) (error 'next-value "no zero diff"))
          (else (loop d (cons s acc)))))))
  (let loop ((v 0) (p 0) (s diff-seqs))
    (if (null? s)
        (cons v p)
        (let ((first (caar s))
              (last (car (reverse (car s)))))
          (loop (+ v last) (- first p) (cdr s))))))

(define example '(
  "0 3 6 9 12 15"
  "1 3 6 10 15 21"
  "10 13 16 21 30 45"
))

(define (solve data)
  (apply + (map car (map next-value (map parse-line data)))))

(assert (= (solve example) 114))

(printf "~a\n" (solve data))

(define (solve2 data)
  (apply + (map cdr (map next-value (map parse-line data)))))

(printf "~a\n" (solve2 data))