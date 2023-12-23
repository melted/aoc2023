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