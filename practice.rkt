#lang racket

(define lst1 (range 10))
(define lst2 '("i" "am" "the" "best" "coder"))

(filter (compose (curry < 2) string-length)
          lst2)

(foldr (lambda (s r)
           (cons (string-length s) r))
         (filter even? lst1)
         lst2)