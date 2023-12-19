(library (parser)

(export parse-fail parse-fail? &parse-fail no-parse satisfy try expect-char
        expect-string parse-sequence parse-one-of parse-some parse-many parse-word
        parse-number whitespace parse-separated-by)
(import (chezscheme))

(define-condition-type &parse-fail &condition make-parse-fail parse-fail?)

(define-record-type no-parse)

(define parse-fail
  (case-lambda 
    ((who msg) (raise (condition
                  (make-parse-fail)
                  (make-who-condition who)
                  (make-message-condition msg))))
    ((who msg port)
       (parse-fail who (string-append msg (format " at position ~a" (port-position port)))))))

(define (satisfy pred)
  (lambda (port)
    (let ((ch (get-char port)))
      (cond
        ((eof-object? ch) (parse-fail 'satisfy "end of file" port))
        ((pred ch) ch)
        (else (parse-fail 'satisfy "satify failed" port))))))

(define (try parser)
  (lambda (port)
    (assert (port-has-port-position? port))
    (assert (port-has-set-port-position!? port))
    (let  ((old-pos (port-position port)))
      (guard
        (ex
          ((parse-fail? ex) (set-port-position! port old-pos)
                            (make-no-parse)))
        (parser port)))))

(define (expect-char ch)
  (lambda (port)
    ((satisfy (lambda (c) (char=? c ch))) port)))

(define (expect-string str)
  (lambda (port)
    (let ((input (get-string-n port (string-length str))))
      (cond
        ((eof-object? input) (parse-fail 'expect-string "end of file" port))
        ((string=? str input) str)
        (else (parse-fail 'expect-string (format "Expected ~a, got ~a" str input) port))))))

(define (parse-sequence . parsers)
  (lambda (port)
    (let loop ((p parsers) (acc '())) 
      (if (null? p)
          (reverse acc)
          (loop (cdr p) (cons ((car p) port) acc))))))

(define (parse-one-of . parsers)
  (lambda (port)
    (let loop ((p parsers))
      (if (null? p) 
        (parse-fail 'parse-one-of "No parser matched" port)
        (let ((res ((try (car p)) port)))
          (if (no-parse? res)
              (loop (cdr p))
              res))))))

(define (parse-some parser)
  (lambda (port)
    (let ((head (parser port)))
      (let ((tail ((parse-many parser) port)))
        (cons head tail)))))

(define (parse-many parser)
  (lambda (port)
    (define out ((try (parse-some parser)) port))
    (if (no-parse? out)
        '()
        out)))

(define parse-word
  (lambda (port)
    (list->string ((parse-some (satisfy char-alphabetic?)) port))))

(define parse-number
  (lambda (port)
    (string->number (list->string ((parse-some (satisfy char-numeric?)) port)))))

(define whitespace
  (lambda (port)
    (begin 
      ((parse-some (satisfy char-whitespace?)) port)
      void)))

(define (parse-separated-by parser sep)
  (lambda (port)
    (let ((res ((try parser) port)))
      (if (no-parse? res)
          '()
          (let loop ((out (list res)))
            (let ((separator ((try (expect-string sep)) port)))
              (if (no-parse? separator)
                  (reverse out)
                  (let ((val (parser port)))
                    (loop (cons val out))))))))))

(define (with-msg who msg parser)
  (lambda (port)
    (guard (ex
      ((parse-fail? ex) (parse-fail who msg port)))
      (parser port))))

)