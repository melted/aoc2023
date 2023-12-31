(import (util) (parser))


;; util
(assert (prefix? "hello" "hell"))
(assert (suffix? "hello" "ello"))
(assert (not (prefix? "hello" "abc")))
(assert (is-substring-at? "splunk" "nk" 4))
(assert (not (is-substring-at? "splunk" "nka" 4)))
(assert (equal? (split-first "a,b,c" ",") '("a" . "b,c")))
(assert (equal? (split "a,b,c" ",") '("a" "b" "c")))
(assert (equal? (bytevector-slice #vu8(100 101 102 103) 1 3) #vu8(101 102)))
(assert (= (string-find "abcdefghijk" "efg") 4))
(assert (= (string-find "abcdefghijk" "ijk") 8))
(assert (= (string-find-string "abcdefghijk" "efg") 4))
(assert (= (string-find-string "abcdefghijk" "ijk") 8))
(assert (= (string-find "abcdefghijk" #\g) 6))
(assert (equal? (interperse 0 '(1 2 3)) '(1 0 2 0 3)))
(assert (string=? (string-join "," "a" "b" "c") "a,b,c"))
(assert (string=? (string-sub "onetwothree" "two" "2") "one2three"))
(assert (equal? (groups '(1 1 0 0)) '((2 . 1) (2 . 0))))

;; parser
(assert (char=? ((satisfy (lambda (c) (char=? #\a))) (open-string-input-port "abc")) #\a))