
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