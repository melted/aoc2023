(import (util))

(define data (list->vector (read-lines-all "./data/input17.txt")))

(define (parse-data data)
  (define (to-vector str)
    (list->vector (map (lambda (ch) (char- ch #\0)) (string->list str))))
  (vector-map to-vector data))

(define (width data) (vector-length (vector-ref data 0)))
(define (height data) (vector-length data))

(define (x pos) (vector-ref pos 0))
(define (y pos) (vector-ref pos 1))

(define (in-bounds? data pos)
  (and (<= 0 (x pos) (- (width data) 1)) (<= 0 (y pos) (- (height data) 1))))

(define (at data pos)
  (if (in-bounds? data pos)
      (vector-ref (vector-ref data (y pos)) (x pos))
      #f))

(define (delta dir)
  (case dir
    ((north) '#(0 -1))
    ((south) '#(0 1))
    ((east) '#(1 0))
    ((west) '#(-1 0))))

(define (pos+ p1 p2)
  (vector (+ (x p1) (x p2))
          (+ (y p1) (y p2))))

(define (pos* n p)
  (vector (* n (x p)) (* n (y p))))

(define (pos= p1 p2)
  (and (= (x p1) (x p2)) (= (y p1) (y p2))))

(define (last3-same? path)
  (or (< (length path) 3)
      (and (eq? (car path) (cadr path))
           (eq? (car path) (caddr path)))))

(define-record-type min-heap
  (fields (mutable data) (mutable used))
  (nongenerative))

(define (create-min-heap)
  (make-min-heap (make-vector 256 #f) 0))

(define (vector-copy-into from to n)
  (do ((i 0 (+ i 1))) ((= n i))
      (vector-set! to i (vector-ref from i))))

(define (grow-heap heap)
  (define new-data (make-vector (* 2 (vector-length (min-heap-data heap)))))
  (vector-copy-into (min-heap-data heap) new-data (min-heap-used heap))
  (min-heap-data-set! heap new-data))

(define (shrink-heap heap)
  (define new-data (make-vector (div (vector-length (min-heap-data heap)) 2)))
  (vector-copy-into (min-heap-data heap) new-data (min-heap-used heap))
  (min-heap-data-set! heap new-data))

(define (parent-index i)
  (div (- i 1) 2))

(define (child-index i)
  (+ (* 2 i) 1))

(define (insert heap el)
  (define used (min-heap-used heap))
  (when (= (vector-length (min-heap-data heap)) used)
        (grow-heap heap))
  (vector-set! (min-heap-data heap) used el)
  (min-heap-used-set! heap (+ used 1))
  (let ((data (min-heap-data heap)))
    (let loop ((i used))
      (when (< 0 i)
            (let ((parent (vector-ref data (parent-index i)))
                  (this (vector-ref data i)))
              (when (< this parent)
                    (vector-set! data (parent-index i) this)
                    (vector-set! data i parent)
                    (loop (parent-index i))))))))

(define (get-min heap)
  (if (> (min-heap-used heap) 0)
      (vector-ref (min-heap-data heap) 0)
      #f))

(define (pop-min heap)
  (define used (- (min-heap-used heap) 1))
  (define data (min-heap-data heap))
  (define out (vector-ref data 0))
  (define (smallest-child i)
    (let ((first-child (child-index i)))
      (cond
        ((<= used first-child) (values #f #f))
        ((= used (+ first-child 1)) (values (vector-ref data first-child) first-child))
        (else
          (let ((fst (vector-ref data first-child))
                (snd (vector-ref data (+ first-child 1))))
            (if (< fst snd)
              (values fst first-child)
              (values snd (+ first-child 1))))))))
  (assert (<= 0 used))
  (when (< used (div (vector-length data) 2))
        (shrink-heap heap)
        (set! data (min-heap-data heap)))
  (vector-set! data 0 (vector-ref data used))
  (min-heap-used-set! heap used)
  (let loop ((i 0))
    (let-values (((val ci) (smallest-child i)))
      (let ((this (vector-ref data i)))
        (if (and ci (< val this))
            (begin
              (vector-set! data ci this)
              (vector-set! data i val)
              (loop ci))
            out)))))


(define (alts data state tmin tmax)
  (define pos (car state))
  (define dp (delta (cadr state)))
  (define future-pos 
    (do ((i 0 (+ i 1))
         (p pos (pos+ p dp))
         (c 0 (+ c (or (at data (pos+ p dp)) 0)))
         (acc '() (cons (cons c p) acc)))
         ((or (not (in-bounds? data p)) (= i (+ 1 tmax))) (list-tail (reverse acc) (min tmin (length acc))))))
  (define turns
    (case (cadr state)
      ((north south) '(east west))
      ((east west) '(north south))))
  (apply append (map (lambda (p) (map (lambda (t) (cons (car p) (list (cdr p) t))) turns)) future-pos)))

(define (search data tmin tmax)
  (define travelled (make-hashtable equal-hash equal?))
  (define frontier (make-eq-hashtable))
  (define (add-state cstate base-cost)
    (let ((state (cdr cstate))
          (cost (+ (car cstate) base-cost)))
      (hashtable-update! frontier cost (lambda (x) (cons state x)) '())))
  (define (pop-state cost)
    (let ((states (hashtable-ref frontier cost #f)))
      (if states
          (begin
            (if (null? (cdr states))
                (hashtable-delete! frontier cost) 
                (hashtable-set! frontier cost (cdr states)))
            (car states))
          #f)))
  (define target (vector (- (width data) 1) (- (height data) 1)))
  (add-state (cons 0 (list (vector 0 0) 'east)) 0)
  (when (= 1 tmin) (add-state (cons 0 (list (vector 0 0) 'south)) 0)) ; Hack for immediate turn on first square
  (let loop ((t 0))
    (let ((min-state (pop-state t)))
      (if min-state
          (begin
            ;(printf "~a ~a ~a ~a\n" t min-state (hashtable-size frontier) (hashtable-size travelled))
            (if (pos= (car min-state) target)
              t
              (let ((prev (hashtable-ref travelled min-state #f)))
                (unless (and prev (<= prev t))
                    (hashtable-set! travelled min-state t)
                    (for-each (lambda (s) (add-state s t))
                          (alts data min-state tmin tmax)))
                (loop t))))
          (loop (+ t 1))))))

(define example '#(
  "2413432311323"
  "3215453535623"
  "3255245654254"
  "3446585845452"
  "4546657867536"
  "1438598798454"
  "4457876987766"
  "3637877979653"
  "4654967986887"
  "4564679986453"
  "1224686865563"
  "2546548887735"
  "4322674655533"
))

(define (solve data)
  (search (parse-data data) 1 3))

(define (solve2 data)
  (search (parse-data data) 4 10))

(assert (= (solve example) 102))

(printf "~a\n" (solve data))
(assert (= (solve2 example) 94))

(printf "~a\n" (solve2 data))