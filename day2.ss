(import (util))

(define data
    (read-lines-all "d:/Niklas/repos/aoc2023/data/input2.txt"))

(define (parse-round str)
  (define rgb (make-vector 3 0))
  (let loop ((fields (split (string-trim str) ",")))
    (if (null? fields)
        rgb
        (let* ((field (split (string-trim (car fields)) " "))
               (index 
                  (cdr (assp (lambda (s) (string=? (cadr field) s))
                    '(("red" . 0)
                      ("green" . 1)
                      ("blue" . 2))))))
            (vector-set! rgb index (string->number (car field)))
            (loop (cdr fields))))))

(define (parse-game str)
  (let* ((xs (split-first str ":"))
         (id (string->number (cadr (split (car xs) " "))))
         (rounds (map parse-round (split (cdr xs) ";"))))
      (cons id rounds)))

(define (victory? game)
  (for-all (lambda (v) (for-all >= '(12 13 14) (vector->list v))) (cdr game)))

(define (wins games)
  (map car (filter victory? games)))

(define (solve1 strs)
  (fold-left + 0 (wins (map parse-game strs))))

(define example '("Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
                  "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue"
                  "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red"
                  "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red"
                  "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"))

(assert (= (solve1 example) 8))

(display (format "~a\n" (solve1 data)))

(define (power game)
  (define max-needed (fold-left (lambda (v1 v2) (vector-map max v1 v2)) (make-vector 3 0) (cdr game)))
  (fold-left * 1 (vector->list max-needed)))


(define (solve2 strs)
  (fold-left + 0 (map power (map parse-game strs))))

(assert (= (solve2 example) 2286))

(display (format "~a\n" (solve2 data)))


