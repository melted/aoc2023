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