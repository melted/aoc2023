(library (util)

(export is-substring-at? prefix? suffix? split-first split bytevector-slice string-find
string-find-naive string-find-string string-join interperse string-sub read-lines-all
string-empty? string-non-empty? string-trim string-left-trim string-right-trim)
(import (chezscheme))

(define (is-substring-at? str what offset)
  (define end (string-length what))
  (if (> (+ end offset) (string-length str))
    #f
    (let loop ((n 0))
      (cond
        ((= n end) #t)
        ((char=? (string-ref what n) (string-ref str (+ n offset))) (loop (+ n 1)))
        (else #f)))))

(define (prefix? str what) (is-substring-at? str what 0))

(define (suffix? str what)
  (is-substring-at? str what (- (string-length str) (string-length what))))

(define (split-first str what)
  (let ((pos (string-find str what)))
    (if pos
      (cons (substring str 0 pos)
            (substring str (+ pos (string-length what)) (string-length str)))
      #f)))

(define (split str what)
  (let loop ((rest str) (acc '()))
    (let ((v (split-first rest what)))
      (if v
        (loop (cdr v) (cons (car v) acc))
        (reverse (cons rest acc))))))

(define (string-find str what)
  (when (not (string? str))
    (error 'string-find "Input must be a string"))
  (cond 
    ((char? what) (string-find-char str what))
    ((string? what) 
        (cond
          ((= (string-length what) 1) (string-find-char str (string-ref what 0)))
          ((< (string-length str) 2000) (string-find-naive str what))
          (else (string-find-string str what))))
    (else (error 'string-find "`what` argument must be a char or a string"))))

(define (string-find-char str what)
  (define len (string-length str))
  (let loop ((pos 0))
    (cond 
      ((= pos len) #f)
      ((char=? (string-ref str pos) what) pos)
      (else (loop (+ pos 1))))))

(define (string-find-naive str what)
  (define end (string-length str))
  (define len (string-length what))
  (let loop ((i 0) (pos 0))
    (cond
      ((= i len) pos)
      ((= pos end) #f)
      ((char=? (string-ref what i) (string-ref str (+ pos i))) (loop (+ i 1) pos))
      (else (loop 0 (+ pos 1))))))

;; Using Boyer-Moore-Horspool
(define (string-find-string str what)
  (define end (string-length str))
  (define len (string-length what))
  (define lookup
    (let ((lookup-table (make-eqv-hashtable len)))
      (let loop ((pos 0))
        (when (< pos (- len 1)) 
          (hashtable-set! lookup-table (string-ref what pos) (- len pos 1))
          (loop (+ pos 1))))
      (lambda (ch)
        (hashtable-ref lookup-table ch len))))
  (define last-ch (string-ref what (- len 1)))
  (define (match pos)
    (let loop ((i 0))
      (cond
        ((= i len) #t)
        ((char=? (string-ref str (- pos i)) (string-ref what (- len i 1))) (loop (+ i 1)))
        (else #f))))
  (let loop ((p (- len 1)))
      (cond
        ((>= p end) #f)
        ((and (char=? last-ch (string-ref str p)) (match p)) (- p len 1))
        (else (loop (+ p (lookup (string-ref str p))))))))

(define (interperse v lst)
  (let loop ((l lst))
    (cond 
      ((null? l) '())
      ((null? (cdr l)) l)
      (else (cons (car l) (cons v (loop (cdr l))))))))

(define (string-join separator . strings)
  (assert (string? separator))
  (assert (for-all string? strings))
  (apply string-append (interperse separator strings)))

(define (string-sub str what that)
  (let ((pieces (split str what)))
    (apply string-join that pieces)))

(define (bytevector-slice bv start end)
  (define len (- end start))
  (define dest (make-bytevector len))
  (bytevector-copy! bv start dest 0 len)
  dest)

(define (read-lines-all path)
  (call-with-input-file path
    (lambda (port)
      (let loop ((lines '()))
        (let ((input (get-line port)))
          (cond
            ((eof-object? input) (reverse (if (string-empty? (car lines)) (cdr lines) lines)))
            (else (loop (cons input lines)))))))))

(define (string-empty? str)
  (= (string-length str) 0))

(define (string-non-empty? str)
  (> (string-length str) 0))

(define (string-trim str)
  (string-left-trim (string-right-trim str)))

(define (string-left-trim str)
  (if (string-non-empty? str)
      (let loop ((i 0))
        (if (char-whitespace? (string-ref str i))
          (loop (+ i 1))
          (substring str i (string-length str))))
      str))


(define (string-right-trim str)
  (if (string-non-empty? str)
      (let loop ((i (string-length str)))
        (if (char-whitespace? (string-ref str (- i 1)))
          (loop (- i 1))
          (substring str 0 i)))
      str))
)
