(import (util))

(define data (list->vector (read-lines-all "./data/input23.txt")))

(define current-data (make-parameter data))

(define (width)
  (string-length (vector-ref (current-data) 0)))

(define (height)
  (vector-length (current-data)))

(define neighbors (list (vector 0 1) (vector 0 -1) (vector 1 0) (vector -1 0)))

(define (x pos) (vector-ref pos 0))
(define (y pos) (vector-ref pos 1))

(define (in-bounds? pos)
  (and (< -1 (x pos) (width)) (< -1 (y pos) (height))))

(define (at pos)
  (if (in-bounds? pos)
      (string-ref (vector-ref (current-data) (y pos)) (x pos))
      #\#))


(define (pos+ p1 p2)
  (vector (+ (x p1) (x p2))
          (+ (y p1) (y p2))))

(define (pos= p1 p2)
  (and (= (x p1) (x p2)) (= (y p1) (y p2))))

(define example '#(
  "#.#####################"
  "#.......#########...###"
  "#######.#########.#.###"
  "###.....#.>.>.###.#.###"
  "###v#####.#v#.###.#.###"
  "###.>...#.#.#.....#...#"
  "###v###.#.#.#########.#"
  "###...#.#.#.......#...#"
  "#####.#.#.#######.#.###"
  "#.....#.#.#.......#...#"
  "#.#####.#.#.#########v#"
  "#.#...#...#...###...>.#"
  "#.#.#v#######v###.###v#"
  "#...#.>.#...>.>.#.###.#"
  "#####v#.#.###v#.#.###.#"
  "#.....#...#...#.#.#...#"
  "#.#########.###.#.#.###"
  "#...###...#...#...#.###"
  "###.###.#.###v#####v###"
  "#...#...#.#.>.>.#.>.###"
  "#.###.###.#.###.#.#v###"
  "#.....###...###...#...#"
  "#####################.#"
))

(define (is-obstacle? pos)
  (char=? (at pos) #\#))

(define (alternatives pos)
  (define ch (at pos))
  (define alts
    (case ch
      ((#\>) (list '#(1 0)))
      ((#\<) (list '#(-1 0)))
      ((#\v) (list '#(0 1)))
      ((#\^) (list '#(0 -1)))
      (else neighbors)))
  (remp is-obstacle? (map (lambda (p) (pos+ pos p)) neighbors)))


(define-record-type hike (fields pos t travelled))


(define (make-graph)
  (define travelled (make-hashtable equal-hash equal?))
  (define nodes (make-hashtable equal-hash equal?))
  (define edges (make-hashtable equal-hash equal?))
  (define start (vector 1 0))
  (define target (vector (- (width) 2) (- (height) 1)))
  (define (add-or-get-node pos)
    (if (hashtable-contains? nodes pos)
        (hashtable-ref nodes pos #f)
        (let ((n (hashtable-size nodes)))
          (hashtable-set! nodes pos n)
          n)))
  (define (has-edge? a b)
    (assq b (hashtable-ref edges a '())))
  (define (add-edge a b t)
    (define (update n)
      (lambda (p)
        (cons (cons n t) p)))
    (unless (or (has-edge? a b) (= a b))
      (hashtable-update! edges a (update b) '())
      (hashtable-update! edges b (update a) '())))
  (hashtable-set! nodes start 0)
  (hashtable-set! nodes target 1)
  (let loop ((current start) (last-node 0) (t 0) (todo '()))
    (let* ((alts  (alternatives current))
           (next (remp (lambda (p) (hashtable-contains? travelled p)) alts)) 
           (len (length next)))
      (for-each (lambda (p)
                  (let ((node? (hashtable-ref nodes p #f)))
                    (when node? (add-edge node? last-node (+ t 1)))))
                alts)
      (cond
        ((= len 0) 
          (when (pos= current target) (add-edge last-node 1 t))
          (if (null? todo)
               (list edges nodes)
               (loop (caar todo) (cdar todo) 1 (cdr todo))))
        ((= len 1)
            (hashtable-set! travelled current #t)
            (loop (car next) last-node (+ t 1) todo))
        (else 
          (let* ((node (add-or-get-node current))
                (items (map (lambda (p) (cons p node)) (cdr next))))
            (add-edge node last-node t)
            (hashtable-set! travelled current #t)
            (loop (car next) node 1 (append items todo))))))))



(define-record-type hike (fields pos t travelled))

(define (split-hike edges)
  (lambda (h)
    (define alts  (remp (lambda (p) (hashtable-contains? (hike-travelled h) (car p)))
                        (hashtable-ref edges (hike-pos h) '())))
    (hashtable-set! (hike-travelled h) (hike-pos h) (hike-t h))
    (map (lambda (p) (make-hike (car p) (+ (hike-t h) (cdr p)) (hashtable-copy (hike-travelled h) #t)))
        alts)))

(define (walk)
  (define edges (car (make-graph)))
  (define start (make-hike 0 0 (make-hashtable equal-hash equal?)))
  (define target 1)
  (let loop ((hikes (list start)) (last 0))
    (if (null? hikes)
        last
        (let ((next (apply append (map (split-hike edges) hikes))))
          (let-values (((finished going) 
                        (partition (lambda (h) (= target (hike-pos h))) next)))
            (printf "~a ~a ~a\n" (length finished) (length going) last)
            (loop going (if (null? finished) last (apply max last (map hike-t finished)))))))))
