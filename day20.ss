(import (util))

(define data (read-lines-all "./data/input20.txt"))

(define-record-type device
  (fields name type connections (mutable state)))

(define (parse-item str)
  (define parts (split-trim str "->"))
  (define targets (split-trim (cadr parts) ","))
  (define type (case (string-ref (car parts) 0)
                  ((#\%) 'flipflop)
                  ((#\&) 'conjunction)
                  (else 'start)))
  (define name (if (eq? type 'start)
                   (car parts)
                   (substring (car parts) 1 (string-length (car parts)))))
  (define state (if (eq? type 'conjunction) (make-hashtable string-hash string=?) #f))
  (make-device name type targets state))

(define (make-circuit devices)
  (define circuit (make-hashtable string-hash string=?))
  (define conjunctions (filter (lambda (d) (eq? (device-type d) 'conjunction)) devices))
  (define (find-inputs conj)
    (define inputs (filter (lambda (d) 
                              (find (lambda (t) (string=? t (device-name conj))) 
                                    (device-connections d))) 
                           devices))
    (for-each (lambda (d) (hashtable-set! (device-state conj) (device-name d) #f)) inputs))
  (for-each find-inputs conjunctions)
  (for-each (lambda (d) (hashtable-set! circuit (device-name d) d)) devices)
  (hashtable-set! circuit "rx" (make-device "rx" 'sink '() #f))
  circuit)

(define (send-pulse circuit pulse)
  (define target (car pulse))
  (define sender (cadr pulse))
  (define level (caddr pulse))
  (define device (hashtable-ref circuit target #f))
  (define destinations (device-connections device))
  (case (device-type device)
    ((flipflop) (if level
                    '()
                    (let ((old-state (device-state device)))
                      (device-state-set! device (not old-state))
                      (map (lambda (d) (list d target (not old-state))) destinations))))
    ((conjunction) (let* ((state (device-state device)))
                      (hashtable-set! state sender level)
                      (let ((signal (not (for-all (lambda (x) x) (vector->list (hashtable-values state))))))
                        (map (lambda (d) (list d target signal)) destinations))))
    ((start) (map (lambda (d) (list d target #f)) destinations))
    ((sink) '())))

(define (execute circuit)
  (define start-pulse (list "broadcaster" "button" #f))
  (let loop ((pulses (list start-pulse)) (next '()) (low 0) (high 0) (rx-triggered? #f))
    (cond
      ((and (null? pulses) (null? next)) (list low high rx-triggered?))
      ((null? pulses) (loop next '() low high rx-triggered?))
      (else (let ((p (car pulses)))
              (let ((out (send-pulse circuit p))
                    (triggered (and (string=? (car p)) (not (caddr p)))))
                (if (caddr p)
                    (loop (cdr pulses) (append next out) low (+ high 1) triggered)
                    (loop (cdr pulses) (append next out) (+ low 1) high triggered))))))))

(define (solve data)
  (define circuit (make-circuit (map parse-item data)))
  (define output (do ((i 0 (+ i 1))
                      (acc '() (cons (execute circuit) acc)))
                     ((= i 1000) acc)))
  (define sums (fold-left (lambda (a p) (cons (+ (car a) (car p)) (+ (cdr a) (cadr p)))) (cons 0 0) output))
  (* (car sums)(cdr sums)))

(define example '(
  "broadcaster -> a, b, c"
  "%a -> b"
  "%b -> c"
  "%c -> inv"
  "&inv -> a"
))

(assert (= (solve example) 32000000))

(define (state-display circuit)
  (define (show-device str)
    (define dev (hashtable-ref circuit str #f))
    (case (device-type dev)
      ((conjunction) 
        (let ((on (length (filter (lambda (x) x) (vector->list (hashtable-values (device-state dev)))))))
          (printf "~a ~a\n" (device-name dev) on)
          on))
      (else 0)))
  (let ((total (apply + (map show-device (list "nh" "kb" "mf" "fd")))))
    (printf "Total ~a\n" total)))

(define (solve2 data)
  (define circuit (make-circuit (map parse-item data)))
  (let loop ((t 1))
    (printf "Round ~a\n" t)
    (state-display circuit)
    (display "\n\n")
    (let ((out (execute circuit)))
      (if (or (caddr out) (= t 1000000))
          t
          (loop (+ t 1))))))