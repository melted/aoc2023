(import (util))

(define data
  (list->vector (read-lines-all "./data/input10.txt")))

(define current-data (make-parameter data))

(define-enumeration direction 
                    (north south east west)
                    directions)

(define (width)
  (string-length (vector-ref (current-data) 0)))

(define (height)
  (vector-length (current-data)))

(define (exits ch)
  (case ch
    ((#\|) (directions north south))
    ((#\-) (directions east west))
    ((#\L) (directions north east))
    ((#\J) (directions north west))
    ((#\7) (directions south west))
    ((#\F) (directions south east))
    ((#\S) (directions north south east west))
    (else #f)))

(define-record-type (pos pos pos?) 
  (fields x y) 
  (nongenerative))

(define (pos-equal? p1 p2)
  (and (= (pos-x p1) (pos-x p2))
       (= (pos-y p1) (pos-y p2))))

(define (pos-hash p)
  (+ (* 293 (pos-x p)) (* 2 (pos-y p))))

(define (pos+ p dp)
  (pos (+ (pos-x p) (pos-x dp))
       (+ (pos-y p) (pos-y dp))))

(define (addp p)
  (lambda (dp) (pos+ p dp)))

(define (pos- p1 p2)
  (pos (- (pos-x p1) (pos-x p2))
       (- (pos-y p1) (pos-y p2))))

(define (delta dir)
  (case dir
    ((north) (pos 0 -1))
    ((south) (pos 0 1))
    ((east) (pos 1 0))
    ((west) (pos -1 0))))

(define (dir delta)
  (cond
    ((pos-equal? delta (pos 0 -1)) 'north)
    ((pos-equal? delta (pos 0 1)) 'south)
    ((pos-equal? delta (pos 1 0)) 'east)
    ((pos-equal? delta (pos -1 0)) 'west)))

(define (complement dir)
  (case dir
    ((north) 'south)
    ((south) 'north)
    ((east) 'west)
    ((west) 'east)))
  
(define (in-bounds? p)
  (and (<= 0 (pos-x p) (- (width) 1))
       (<= 0 (pos-y p) (- (height) 1))))

(define (at p)
  (if (in-bounds? p)
      (string-ref (vector-ref (current-data) (pos-y p)) (pos-x p))
      #\.))

(define (connected? p dir)
  (define dest (exits (at (pos+ p (delta dir)))))
  (if dest
      (enum-set-member? (complement dir) dest)
      #f))

(define (destinations p)
  (let loop ((d (enum-set->list (exits (at p)))) (ways '()))
    (cond 
      ((null? d) (reverse ways))
      ((connected? p (car d)) (loop (cdr d) (cons (pos+ p (delta (car d))) ways)))
      (else (loop (cdr d) ways)))))

(define (find-start)
  (exists (lambda (s i) (let ((x (string-find s #\S)))
                          (if x (pos x i) #f)))
          (vector->list (current-data)) 
          (iota (height))))

(define (search start destinations)
  (define traversed (make-hashtable pos-hash pos-equal?))
  (let loop ((todo start) (t 0))
    (if (null? todo)
        traversed
        (let ((reachable
                (apply append (map destinations todo))))
          (for-each (lambda (p) (hashtable-set! traversed p t)) todo)
          (let ((next 
                  (filter (lambda (p) (not (hashtable-contains? traversed p)))
                          reachable)))
            (loop next (+ t 1)))))))

(define (search-loop)
  (search (list (find-start)) destinations))

(define example '#(
    "7-F7-"
    ".FJ|7"
    "SJLL7"
    "|F--J"
    "LJ.LJ"))

(define (solve)
  (let-values (((keys values) (hashtable-entries (search-loop))))
    (apply max (vector->list values))))

(assert (= (parameterize ((current-data example)) (solve)) 8))

(printf "~a\n" (solve))

(define (tiles ch p)
  (case ch
    ((#\-) (list (list (pos 0 1)) (list (pos 0 -1))))
    ((#\|) (list (list (pos 1 0)) (list (pos -1 0))))
    ((#\L) (list '() (list (pos 0 1) (pos -1 0))))
    ((#\7) (list '() (list (pos 1 0) (pos 0 -1)) ))
    ((#\F) (list (list (pos -1 0) (pos 0 -1)) '()))
    ((#\J) (list (list (pos 1 0) (pos 0 1)) '()))
    ((#\S) (tiles #\L p))
    (else (list '() '()))))

(define (paint loop)
  (define left (make-hashtable pos-hash pos-equal?))
  (define right (make-hashtable pos-hash pos-equal?))
  (define start (find-start))
  (define (add-tile ht) (lambda (p)
    (unless (hashtable-contains? loop p) 
        (hashtable-set! ht p #t))))
  (define end (car (destinations start)))
  (let walk ((p start) (trip (list end)))
    (let* ((dests (destinations p))
           (last (car trip))
           (first-dir? (pos-equal? (car dests) last))
           (next (if first-dir? (cadr dests) (car dests)))
           (t (tiles (at p) p))
           (right-tiles (if first-dir? (car t) (cadr t)))
           (left-tiles (if first-dir? (cadr t) (car t))))
      (for-each (add-tile right) (map (addp p) right-tiles))
      (for-each (add-tile left) (map (addp p) left-tiles))
      (if (pos-equal? next start)
          (list right left trip)
          (walk next (cons p trip))))))

(define neighbors (list (pos 0 1) (pos 0 -1) (pos 1 0) (pos -1 0)))

(define (floodfill loop filler)
  (define next (hashtable-copy filler #t))
  (define (add-tile p)
    (when (valid? p) (hashtable-set! next p #t)))
  (define (valid? p)
    (and (in-bounds? p) (not (hashtable-contains? loop p)) (not (hashtable-contains? filler p))))
  (let fill ()
    (if (= 0 (hashtable-size next))
        filler
        (let ((last (vector->list (hashtable-keys next))))
          (for-each (lambda (p) (hashtable-set! filler p #t)) last)
          (hashtable-clear! next)
          (for-each add-tile (apply append (map (lambda (p) (map (addp p) neighbors)) last)))
          (fill)))))

(define (solve2)
  (define l (search-loop))
  (define p (paint l))
  (define right (floodfill l (car p)))
  (define left (floodfill l (cadr p)))
  (show l (cadr p) (car p))
  (list (hashtable-size right) (hashtable-size left) (hashtable-size loop)))

(define (show bound l r)
  (for-each 
    (lambda (y)
      (let ((str (make-string (width) #\space)))
        (for-each (lambda (x)
          (when (hashtable-contains? bound (pos x y))
            (string-set! str x #\x))
          (when (hashtable-contains? l (pos x y))
            (string-set! str x #\.))
          (when (hashtable-contains? r (pos x y))
            (string-set! str x #\-)))
                  (iota (width)))
        (display str) (newline)))
    (iota (height))))

(define example2 '#(
  "FF7FSF7F7F7F7F7F---7"
"L|LJ||||||||||||F--J"
"FL-7LJLJ||||||LJL-77"
"F--JF--7||LJLJ7F7FJ-"
"L---JF-JLJ.||-FJLJJ7"
"|F|F-JF---7F7-L7L|7|"
"|FFJF7L7F-JF7|JL---7"
"7-L-JL7||F7|L7F-7F7|"
"L.L7LFJ|||||FJL7||LJ"
"L7JLJL-JLJLJL--JLJ.L"
))

(define example3 '#(
  ".........."
".S------7."
".|F----7|."
".||....||."
".||....||."
".|L-7F-J|."
".|..||..|."
".L--JL--J."
".........."
))

;(assert (= (parameterize ((current-data example3)) (solve2)) 4))


(printf "~a\n" (solve2))