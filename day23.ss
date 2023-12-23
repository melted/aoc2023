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

(define (split-hike h)
  (define alts  (remp (lambda (p) (hashtable-contains? (hike-travelled h) p))
                      (alternatives (hike-pos h))))
  (hashtable-set! (hike-travelled h) (hike-pos h) (hike-t h))
  (map (lambda (p) (make-hike p (+ (hike-t h) 1) (hashtable-copy (hike-travelled h) #t)))
       alts))

(define (walk)
  (define start (make-hike (vector 1 0) 0 (make-hashtable equal-hash equal?)))
  (define target (vector (- (width) 2) (- (height) 1)))
  (let loop ((hikes (list start)) (t 0))
    (if (null? hikes)
        t
        (let ((next (apply append (map split-hike hikes))))
          (let-values (((finished going) 
                        (partition (lambda (h) (pos= target (hike-pos h))) next)))
            (loop going (+ t 1)))))))