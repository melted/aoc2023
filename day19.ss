(import (util) (parser))

(define data (split (call-with-input-file "./data/input19.txt" get-string-all) "\n\n"))

(define-record-type clause
  (fields attr op num goto)
  (nongenerative))

(define-record-type rule
  (fields name clauses default)
  (nongenerative))

(define (parse-data data)
  (define rules (split (car data) "\n"))
  (define items (split-trim (cadr data) "\n"))
  (values (map parse-rule rules)
          (map parse-item items)))

(define (attribute->number attr)
  (case attr
    (("x") 0)
    (("m") 1)
    (("a") 2)
    (("s") 3)))

(define (parse-clause port)
  (let ((attribute (parse-word port)))
    (let ((op ((parse-one-of (expect-char #\<) (expect-char #\>)) port)))
      (let ((num (parse-number port)))
        ((expect-char #\:) port)
        (let ((next (parse-word port)))
          ((expect-char #\,) port)
          (make-clause (attribute->number attribute) op num next))))))

(define (parse-rule str)
  (define port (open-string-input-port str))
  (let ((name (parse-word port)))
    ((expect-char #\{) port)
    (let ((clauses ((parse-some parse-clause) port)))
      (let ((end (parse-word port)))
        ((expect-char #\}) port)
        (make-rule name clauses end)))))

(define (parse-item str)
  (define port (open-string-input-port str))
  ((expect-string "{x=") port)
  (let ((x (parse-number port)))
    ((expect-string ",m=") port)
    (let ((m (parse-number port)))
      ((expect-string ",a=") port)
      (let ((a (parse-number port)))
        ((expect-string ",s=") port)
        (let ((s (parse-number port)))
          (vector x m a s))))))


(define (execute rules items)
  (define rule-db (make-hashtable equal-hash equal?))
  (for-each (lambda (r) (hashtable-set! rule-db (rule-name r) r)) rules)
  (filter (lambda (i) (eq? (handle rule-db "in" i) 'accept)) items))

(define (handle rule-db name item)
  (define rule (hashtable-ref rule-db name #f))
  (define outcome 
    (let loop ((c (rule-clauses rule)))
      (if (null? c)
          (rule-default rule)
          (let ((res (apply-clause (car c) item)))
            (if res
                res
                (loop (cdr c)))))))
  (case outcome
    (("A") 'accept)
    (("R") 'reject)
    (else (handle rule-db outcome item))))

(define (apply-clause clause item)
  (define val (vector-ref item (clause-attr clause)))
  (if ((if (char=? (clause-op clause) #\<) < >) val (clause-num clause))
      (clause-goto clause)
      #f))

(define (solve data)
  (define-values (rules items) (parse-data data))
  (apply + (map (lambda (v) (apply + (vector->list v))) (execute rules items))))

(define (sort-range range)
  (list-sort (lambda (a b) (< (car a) (car b))) range))

(define (split-range range val)
  (let loop ((r range) (lower '()) (higher '()))
    (if (null? r)
        (list (sort-range lower) (sort-range higher))
        (let ((piece (car r)))
          (cond
            ((< val (car piece)) (loop '() lower (append higher r)))
            ((<= (car piece) val (- (cdr piece) 1))
               (loop (cdr r)
                     (cons (cons (car piece) val) lower)
                     (cons (cons (+ val 1) (cdr piece)) higher)))
            ((< (cdr piece) val) (loop (cdr r) (cons piece lower) higher)))))))

(define (valid-ranges? ranges) 
  (if (exists null? (vector->list ranges))
      #f
      ranges))

(define (do-clause clause ranges)
  (define range (vector-ref ranges (clause-attr clause)))
  (define v (vector-copy ranges))
  (define less-than? (char=? (clause-op clause) #\<))
  (define val (if less-than? (- (clause-num clause) 1) (clause-num clause)))
  (define new (split-range range val))
  (vector-set! ranges (clause-attr clause) (car new))
  (vector-set! v (clause-attr clause) (cadr new))
  (if less-than?
      (values (cons (clause-goto clause) (valid-ranges? ranges)) (valid-ranges? v))
      (values (cons (clause-goto clause) (valid-ranges? v)) (valid-ranges? ranges))))

(define (range-value range)
  (fold-left + 0 (map (lambda (x) (+ (- (cdr x) (car x)) 1)) range)))

(define (score ranges)
  (apply * (map range-value (vector->list ranges))))

(define (do-rule rule-db name ranges)
  (define rule (hashtable-ref rule-db name #f))
  (define todo
    (let loop ((c (rule-clauses rule)) (left ranges) (todo '()))
      (if (null? c)
          (if left 
              (cons (cons (rule-default rule) left) todo) 
              todo)
          (let-values (((taken untaken) (do-clause (car c) left)))
            (loop (cdr c) untaken (cons taken todo))))))
  (let loop ((work (filter (lambda (x) x) todo)) (total 0))
    (if (null? work)
        total
        (let ((item (car work)))
          (case (car item)
            (("A") (loop (cdr work) (+ total (score (cdr item)))))
            (("R") (loop (cdr work) total))
            (else (loop (cdr work) (+ total (do-rule rule-db (car item) (cdr item))))))))))

(define (solve2 data)
  (define-values (rules items) (parse-data data))
  (define start-ranges (vector (list (cons 1 4000))
                               (list (cons 1 4000))
                               (list (cons 1 4000))
                               (list (cons 1 4000))))
  (define rule-db (make-hashtable equal-hash equal?))
  (for-each (lambda (r) (hashtable-set! rule-db (rule-name r) r)) rules)
  (do-rule rule-db "in" start-ranges))

(define example '(
"px{a<2006:qkq,m>2090:A,rfg}
pv{a>1716:R,A}
lnx{m>1548:A,A}
rfg{s<537:gd,x>2440:R,A}
qs{s>3448:A,lnx}
qkq{x<1416:A,crn}
crn{x>2662:A,R}
in{s<1351:px,qqz}
qqz{s>2770:qs,m<1801:hdj,R}
gd{a>3333:R,R}
hdj{m>838:A,pv}"

"{x=787,m=2655,a=1222,s=2876}
{x=1679,m=44,a=2067,s=496}
{x=2036,m=264,a=79,s=2244}
{x=2461,m=1339,a=466,s=291}
{x=2127,m=1623,a=2188,s=1013}"
))

(assert (= (solve example) 19114))


(assert (= (solve2 example) 167409079868000))

(printf "~a\n" (solve data))

(printf "~a\n" (solve2 data))