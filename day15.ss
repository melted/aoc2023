(import (util))

(define data (string-trim (call-with-input-file "./data/input15.txt" get-string-all)))

(define example "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7")

(define (hash str)
  (define (hash-char h ch)
    (mod (* (+ h (char->integer ch)) 17) 256))
  (fold-left hash-char 0 (string->list str)))

(define (solve data)
  (apply + (map hash (split data ","))))

(assert (= (solve example) 1320))

(printf "~a\n" (solve data))

(define (op str)
  (define sliced (string-take-while str char-alphabetic?))
  (define name (car sliced))
  (define rest (cadr sliced))
  (define operator (string-ref rest 0))
  (case operator
    ((#\=) (list 'assign name (string->number (substring rest 1 (string-length rest)))))
    ((#\-) (list 'remove name))))

(define (solve2 data)
  (define ops (map op (split data ",")))
  (define boxes (make-vector 256 '()))
  (define (handle-op x)
    (case (car x)
      ((assign) (assign-op (cadr x) (caddr x)))
      ((remove) (remove-op (cadr x)))))
  (define (box-name? name) 
    (lambda (x) 
      (string=? (vector-ref x 0) name)))
  (define (assign-op name focus)
    (let* ((i (hash name))
           (b (vector-ref boxes i))
           (val (find (box-name? name) b)))
      (if val
          (vector-set! val 1 focus)
          (vector-set! boxes i (cons (vector name focus) b)))))
  (define (remove-op name)
    (let* ((i (hash name))
           (b (vector-ref boxes i)))
      (vector-set! boxes i (remp (box-name? name) b))))
  (define (score i)
    (let ((ls (reverse (vector-ref boxes i))))
      (do ((n 1 (+ n 1)) 
           (el ls (cdr el))
           (acc 0 (+ acc (* n (vector-ref (car el) 1)))))
          ((null? el) (* (+ i 1) acc)))))
  (for-each handle-op ops)
  (apply + (map score (iota 256))))

  
(assert (= (solve2 example) 145))

(printf "~a\n" (solve2 data))