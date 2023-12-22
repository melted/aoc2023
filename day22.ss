(import (util))

(define data (list->vector (read-lines-all "./data/input22.txt")))

(define (parse-line str)
  (define ends (split str "~"))
  (define vs (map (lambda (s) (apply vector (map string->number (split s ",")))) ends))
  vs)

(define (at arena pos)
  (vector-ref (vector-ref arena (z pos)) (+ (* 10 (y pos)) (x pos))))

(define (set-at! arena x y z what)
  (vector-set! (vector-ref arena z) (+ (* 10 y) x) what))

(define (x pos) (vector-ref pos 0))
(define (y pos) (vector-ref pos 1))
(define (z pos) (vector-ref pos 2))

(define (under pos) (vector (x pos) (y pos) (- (z pos) 1)))
(define (over pos) (vector (x pos) (y pos) (+ (z pos) 1)))


(define (brick->list brick)
  (define from (car brick))
  (define to (cadr brick))
  (let loop ((d 0) (acc (list from)))
    (if (= d 3)
        acc
        (let* ((new (map (lambda (i) 
                            (let ((nv (vector-copy from)))
                              (vector-set! nv d (+ (vector-ref nv d) i 1))
                              nv))
                         (iota (- (vector-ref to d) (vector-ref from d))))))
          (loop (+ d 1) (append acc new))))))


(define (set-brick! arena brick what)
  (define cubes (brick->list brick))
  (for-each (lambda (c) (set-at! arena (x c) (y c) (z c) what)) cubes))

(define (make-arena bricks)
  (define max-z (fold-left (lambda (a b) (max a (vector-ref (car b) 2) (vector-ref (cadr b) 2))) 0 (vector->list bricks)))
  (define arena (make-vector (+ max-z 1)))
  (for-each (lambda (i) (vector-set! arena i (make-vector 100 #f))) (map add1 (iota max-z)))
  (vector-set! arena 0 (make-vector 100 99999))
  arena)

(define (fill-arena arena bricks)
  (let loop ((i 0))
    (if (= i (vector-length bricks))
        arena
        (begin
          (set-brick! arena (vector-ref bricks i) i)
          (loop (+ i 1))))))

(define (supported-by arena brick)
    (let* ((cubes (brick->list brick))
           (supports (remp (lambda (c) (member c cubes)) (map under cubes))))
      (nub (list-sort < (filter number? (map (lambda (p) (at arena p)) supports))))))

(define (settle arena bricks)
  (define sorted (list-sort (lambda (a b) 
                              (< (z (car (vector-ref bricks a))) 
                                             (z (car (vector-ref bricks b)))))
                            (iota (vector-length bricks))))
  (define fallen 0)
  (define (fall brick i)
    (if (null? (supported-by arena brick))
        (let ((new-brick (list (under (car brick)) (under (cadr brick)))))
          (set-brick! arena brick #f)
          (set-brick! arena new-brick i)
          (vector-set! bricks i new-brick)
          (fall new-brick i)
          #t)
        #f))
  (let loop ((l sorted) (acc '()))
    (if (null? l)
        (values acc fallen)
        (let ((i (car l)))
          (when (fall (vector-ref bricks i) i)
                (set! fallen (+ fallen 1)))
          (let ((supps (supported-by arena (vector-ref bricks i))))
            (loop (cdr l) (cons (list i supps) acc)))))))
    
(define (solve data)
  (define bricks (vector-map parse-line data))
  (define arena (make-arena bricks))
  (fill-arena arena bricks)
  (let-values (((supps fallen) (settle arena bricks)))
    (let ((unsafe (list-sort < (map caadr (filter (lambda (x) (and (= (length (cadr x)) 1) (< (caadr x) 99999))) supps)))))
      (- (vector-length bricks) (length (nub unsafe))))))

(define example '#(
  "1,0,1~1,2,1"
  "0,0,2~2,0,2"
  "0,2,3~2,2,3"
  "0,0,4~0,2,4"
  "2,0,5~2,2,5"
  "0,1,6~2,1,6"
  "1,1,8~1,1,9"
))

(assert (= (solve example) 5))

(printf "~a\n" (solve data))

(define (nuke data) 
  (lambda (brick)
    (define bricks (vector-map parse-line data))
    (define arena (make-arena bricks))
    (fill-arena arena bricks)
    (settle arena bricks)
    (set-brick! arena (vector-ref bricks brick) #f)
    (let-values (((supp fallen) (settle arena bricks)))
      fallen)))

(define (solve2 data)
  (let ((res (map (nuke data) (iota (vector-length data)))))
    (apply + res)))

(assert (= (solve2 example) 7))

(printf "~a\n" (solve2 data))