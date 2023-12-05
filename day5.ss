(import (util))

(define data
 (split 
    (call-with-input-file  "./data/input5.txt" get-string-all)
    "\n\n"))

(define (seeds data)
  (map string->number (cdr (split-trim (car data) " "))))

(define-record-type entry
  (fields start end offset))

(define (create-entry dst src range)
  (make-entry src (+ src range) (- dst src)))

(define (get-seeds data)
  (map (lambda (x) (make-entry x (+ x 1) 0)) (seeds data)))

(define (get-seeds2 data)
  (let loop ((s (seeds data)) (acc '()))
    (if (null? s)
        (reverse acc)
        (loop (cddr s) (cons (make-entry (car s) (+ (car s) (cadr s)) 0) acc)))))

(define (make-map str)
  (define lines (cdr (split-trim str "\n")))
  (define (parse-line l)
    (apply create-entry (map string->number (split-trim l " "))))
  (map parse-line lines))

(define (parse-maps data)
  (map make-map (cdr data)))

(define (overlap? seed1 seed2)
  (not (or (> (entry-start seed1) (entry-end seed2))
           (> (entry-start seed2) (entry-end seed1)))))

(define (split-on seed splitter)
  (values
    (if (< (entry-start seed) (entry-start splitter))
        (make-entry (entry-start seed) (min (entry-end seed) (entry-start splitter)) 0)
        #f)
    (if (overlap? seed splitter)
        (make-entry (max (entry-start seed) (entry-start splitter))
                    (min (entry-end seed) (entry-end splitter))
                    0)
        #f)
    (if (> (entry-end seed) (entry-end splitter)) 
        (make-entry (max (entry-start seed) (entry-end splitter)) (entry-end seed) 0)
        #f)))

(define (coalesce inputs)
  (define sorted (list-sort (lambda (a b) (< (entry-start a) (entry-start b))) inputs))
  (if (null? sorted)
      sorted
      (let loop ((current (car sorted)) (rest (cdr sorted)) (out '()))
        (if (null? rest)
            (reverse (cons current out))
            (let ((candidate (car rest)))
              (if (overlap? current candidate)
                (loop (make-entry 
                        (min (entry-start current) (entry-start candidate))
                        (max (entry-end current) (entry-end candidate))
                        0)
                      (cdr rest)
                      out)
                (loop (car rest) (cdr rest) (cons current out))))))))

(define (remap seed entry)
  (if (overlap? seed entry)
    (let-values (((low to-move high) (split-on seed entry)))
      (let* ((n (entry-offset entry))
           (moved (make-entry (+ (entry-start to-move) n) (+ (entry-end to-move) n) 0)))
        (values (filter entry? (list moved)) (filter entry? (list low high)))))
    (values '() (list seed))))
        
(define (map-pass the-map input)
  (let loop ((m the-map) (remaining input) (out '()))
    (if (null? m)
        (coalesce (append remaining out))
        (let* ((p (car m)))
          (let inner ((seeds remaining) (mapped '()) (unmapped '()))
            (if (null? seeds)
                (loop (cdr m) unmapped (append mapped out))
                (let-values (((mv umv) (remap (car seeds) p)))
                  (inner (cdr seeds) (append mv mapped) (append umv unmapped)))))))))

(define (pass-seeds maps seed)
  (fold-left (lambda (s m) (map-pass m s)) seed maps))

(define example
  '("seeds: 79 14 55 13"

"seed-to-soil map:
50 98 2
52 50 48"

"soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15"

"fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4"

"water-to-light map:
88 18 7
18 25 70"

"light-to-temperature map:
45 77 23
81 45 19
68 64 13"

"temperature-to-humidity map:
0 69 1
1 0 69"

"humidity-to-location map:
60 56 37
56 93 4"))

(define (solve data seed-fn)
  (define maps (parse-maps data))
  (apply min (map entry-start (pass-seeds maps (seed-fn data)))))

(assert (= (solve example get-seeds) 35))

(printf "~a\n" (solve data get-seeds))

(assert (= (solve example get-seeds2) 46))

(printf "~a\n" (solve data get-seeds2))