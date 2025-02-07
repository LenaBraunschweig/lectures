#lang racket

(require racket/trace)

(define (foo x)
  (* 2 x))

(define (bar y)
  (+ y 100))

(define (repeat n x)
  (if (= n 0) 
    '()
    (cons(x (repeat (sub1 n) x)))))

(define (flip f)
  (lambda (x y) (f y x)))

#|-----------------------------------------------------------------------------
;; Higher-order functions (HOFs)

HOFs either take a function as an argument or return a function.
  - a call to a lambda is technically an HOF
  - requires a first order function; aka one that acts as a value
    - also called first class functions

Some useful built-in HOFs and related functions:

- `apply`: apply a function to a list of arguments
  - ex. (apply + '(1 2 3))
  - can have multiple arguments as long as the last argument is a list

- `curry`: returns a version of a function that can be partially applied
  - curried version will run even if the original function would not
  - if you don't have enough arguments, curry will take its parameters one at a time
  - ex. (define r5 ((curry repeat) 5)
  - allows you to run functions with atleast 1 argument already specified

- `compose`: returns a function that is the composition of two other functions
  - puts the output of the second function into the first one
  - ex. ((compose foo bar) 5) is the same as (foo (bar 5))

- `eval`: evaluates a sexp
  - good for when you have data structures
-----------------------------------------------------------------------------|#

;; `apply` applies a function to lists
(apply + '(1 2 3))
(apply + 1 2 '(3))

;; `curry` gives us partial application
(define thrice ((curry repeat) 3))

;; compose is a simple but powerful form of "functional "glue"
(define sabs (compose sqrt abs)) ;; applies abs first, going right to left in the functions

;; eval is like having access to the Racket compiler in Racket!
(eval '(+ 1 2))

(define (my-if test e1 e2)
  (eval `(cond [,test ,e1]
         [else ,e2])))

(my-if '(< 1 0)
  '(println "true")
  '(println "false"))


;; delay creates a promise that the sexp will be evaluated later
;; only computes once, but caches the answer to use for later
(define p (delay (+ 1 2)))

;; force
(force p)

(struct mypromise (thunk val) #:mutable)

(define (my-delay sexp)
  (mypromise (lambda () (eval sexp)) #f)) ;; delaying evaluation using a lambda

(define (my-force p)
  (if (not (mypromise-val p))
  (let ([res ((mypromise-thunk p))]) ;; extra pair of parantheses runs the code
    (set-mypromise-val! p res)
    res)
    (mypromise-val p)))

#|-----------------------------------------------------------------------------
;; Some list-processing HOFs

- `map`: applies a function to every element of a list, returning the results
  - ex. (map add1 '(1 3 8))
  - ex. (map (curry * 2) '(2 5 10))
    - curry would be like (* 2 #) for every item in the list

- `filter`: collects the values of a list for which a predicate tests true

- `foldr`: implements *primitive recursion*

- `foldl`: like `foldr`, but folds from the left; tail-recursive
-----------------------------------------------------------------------------|#

;; `map` examples
#; (values
   (map add1 (range 10))

   (map (curry * 2) (range 10))
 
   (map string-length '("hello" "how" "is" "the" "weather?")))

(define (map f lst)
  (if (empty? lst)
    '()
    (cons (f (first lst)) (map f (rest lst)))))

;; `filter` examples
#; (values 
   (filter even? (range 10))
   
   (filter (curry < 5) (range 10)) ;; curry works like (< 5 ?)

   (filter (compose (curry equal? "hi")
                    car)
           '(("hi" "how" "are" "you")
             ("see" "you" "later")
             ("hi" "med" "low")
             ("hello" "there"))))

(filter even? (map (curry + 5) (range 20)))


;; `foldr` examples
#; (values
    (foldr + 0 (range 10))

    (foldr cons '() (range 10))

    (foldr cons '(a b c d e) (range 5))

    (foldr (lambda (x acc) (cons x acc)) ; try trace-lambda
           '()
           (range 5)))

(foldr + 0 (range 10))

#;
(define (summation lst)
  (if (empty? lst)
    0
    (+ (first lst) (summation (rest lst)))))


;; definition of foldr
(define (foldr op v lst)
  (if (empty? lst)
    v
    (op (first lst) (foldr op v (rest lst)))))

(define summation (curry foldr + 0))
(define concat (curry foldr cons))


;; `foldl` examples
#; (values
    (foldl + 0 (range 10))
    
    (foldl cons '() (range 10))
    
    (foldl cons '(a b c d e) (range 5))
    
    (foldl (lambda (x acc) (cons x acc)) ; try trace-lambda
           '()
           (range 5)))

;; foldl definition
(define (foldl op acc lst)
  (if (empty? lst)
    acc
    (foldl op (op (first lst) acc) (rest lst))))

#|-----------------------------------------------------------------------------
;; Lexical scope

- A free variable is bound to a value *in the environment where it is defined*, 
  regardless of when it is used

- This leads to one of the most important ideas we'll see: the *closure*
-----------------------------------------------------------------------------|#