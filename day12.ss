(import (util))

(define data
  (read-lines-all "./data/input12.txt"))

(define (parse-line str)
  (define row-counts (split-first str " "))
  (define counts (map string->number (split-trim (cdr row-counts) ",")))
  (cons (car row-counts) (list->vector counts)))

(define (solutions entry)
  (define row (car entry))
  (define row-length (string-length row))
  (define counts (cdr entry))
  (define count-length (vector-length counts))
  (define states (make-hashtable equal-hash equal?))
  (define possibilities 0)
  (define start (list (cons (vector 0 0 0 #f) 1)))
  (define (add-state v c)
    (hashtable-update! states 
                       v
                       (lambda (x) (+ x c))
                       0))
  (let loop ((prev start))
    (hashtable-clear! states)
    (if (null? prev)
        possibilities
        (begin
        (let inner ((s prev))
          (if (null? s)
              (loop (vector->list (hashtable-cells states)))
              (let* ((current-state (caar s))
                     (state-count (cdr (car s)))
                     (row-index (vector-ref current-state 0))
                     (count-index (vector-ref current-state 1))
                     (count-n (vector-ref current-state 2))
                     (need-dot (vector-ref current-state 3))
                     (row-value (if (< row-index row-length) (string-ref row row-index) #\X)))
                (cond
                  ((= row-index row-length) 
                    (when (= count-index count-length)
                          (set! possibilities (+ possibilities state-count))))
                  ((and (or (char=? row-value #\#) (char=? row-value #\?)) (< count-index count-length) (not need-dot))
                    (when (and (char=? row-value #\?) (= count-n 0))
                          (add-state (vector (+ row-index 1) count-index count-n need-dot) state-count))
                    (if (= (+ count-n 1) (vector-ref counts count-index))
                        (add-state (vector (+ row-index 1) (+ count-index 1) 0 #t) state-count)
                        (add-state (vector (+ row-index 1) count-index (+ count-n 1) #f) state-count)))
                  ((and (or (char=? row-value #\.) (char=? row-value #\?)) (= count-n 0))
                    (add-state (vector (+ row-index 1) count-index 0 #f) state-count)))
                (inner (cdr s)))))))))

(define example '(
"???.### 1,1,3"
".??..??...?##. 1,1,3"
"?#?#?#?#?#?#?#? 1,3,1,6"
"????.#...#... 4,1,1"
"????.######..#####. 1,6,5"
"?###???????? 3,2,1"))

(define (solve data)
  (apply +(map solutions (map parse-line data))))

(assert (= (solve example) 21))

(printf "~a\n" (solve data))

(define (magnify entry)
  (cons (apply string-join "?" (make-list 5 (car entry))) 
        (list->vector (apply append (make-list 5 (vector->list (cdr entry)))))))

(define (solve2 data)
  (apply +(map solutions (map magnify (map parse-line data)))))

(assert (= (solve2 example) 525152))

(printf "~a\n" (solve2 data))


