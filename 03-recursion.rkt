#lang racket

(require racket/trace) ; for dynamic function call traces

#|-----------------------------------------------------------------------------
;; Recursion

- what is a recursive function?
- a function that calls itself

- what are some common "rules" for writing recursive functions?
  - have a base (terminal) case
  - each recursive cal should be solving a "sub-problem" of the original
  - idea: make profress towards base case in each recursive application
    - ensures termination
-----------------------------------------------------------------------------|#

;; Factorial: n! = n * (n - 1) * (n - 2) * ... * 1
(define (factorial n)
  (if (= n 1)
    1 
    (* n (factorial (sub1 n)))))

(trace factorial)

; (trace factorial)

;; Integer summation: m + (m + 1) + (m + 2) + ... + n
(define (sum-from-to m n)
  (if (> m n)
    0
    (+ m (sum-from-to (add1 m) n))))

(trace sum-from-to)

; (trace sum-from-to)

#|-----------------------------------------------------------------------------
 The above functions demonstrate *structural recursion*, because they recurse
 over the structure of the input data.
 
 We can represent natural numbers as self-referential structures to more
 explicitly demonstrate structural recursion.
-----------------------------------------------------------------------------|#

(struct Z () #:transparent)     ; zero
(struct S (pred) #:transparent) ; "successor" of pred

(define one   (S (Z)))
(define two   (S one))
(define three (S two))
(define four  (S three))

;; What is the general form of a structurally recursive function over the
;; natural numbers?
(define (nats-rec-form n)
  (cond [(Z? n) '...] ;base case
        [(S? n)('...(nats-rec-form(S-pred n)) '...)]))

;; Add two natural numbers
;; add(m, n) = n, if m = 0
;;           = 1 + add(m-1, n), otherwise
(define (add-nats m n)
  (cond [(Z? m) n] ;base case
        [(S? m)(S (add-nats (S-pred m) n))])) ;; finds the successor

#|-----------------------------------------------------------------------------
;; Proofs of correctness

- how can we prove that a structurally recursive function is correct?
  - a variation on proofs by induction
    1. prove the function is correct for the base case
    2. assume that the function is correct for case k, prove that it will
       also be correct for case k + 1
    3. conclude that the function works for all cases
  
  - the variation for us is the proof by "structural induction"

- can we apply this to `factorial` and the other functions above?
-----------------------------------------------------------------------------|#

#|-----------------------------------------------------------------------------
;; Tail recursion and Accumulators

- what is tail recursion?
  - the very last bit of computation is the recursive call itself

- what is an accumulator?
  - hangs onto the intermediate result of final
  - normally, the stack frame keeps track of the intermediates

- why would we use these techniques?
-----------------------------------------------------------------------------|#

(define (factorial-tail n [acc 1])
  (if (= n 1)
    acc
    (factorial-tail (sub1 n) (* n acc))))

(trace factorial-tail)

#;
(define (sum-from-to m n)
  (if (> m n)
    0
    (+ m (sum-from-to (add1 m) n))))

(define (sum-from-to-tail m n [acc 0])
  (if (> m n)
    acc
    (sum-from-to-tail (add1 m) n (+ m acc))))

(trace sum-from-to-tail)

#;
(define (add-nats m n)
  (cond [(Z? m) n] ;base case
        [(S? m)(S (add-nats (S-pred m) n))])) ;; finds the successor

(define (add-nats-tail m n [acc n])
  (cond [(Z? m) acc]
        [(S? m) (add-nats-tail (S-pred m) n (S acc))]))

(trace add-nats-tail)

#|-----------------------------------------------------------------------------
;; Structural recursion on lists
-----------------------------------------------------------------------------|#

;; length: the number of elements in a list
(define (length lst)
  (if (empty? lst)
    0
    (add1 (length (rest lst)))))

(define (length-tail lst [acc 0])
  (if (empty? lst)
    acc
    (length (rest lst) (add1 acc))))

;; concat: concatenate the elements of two lists
(define (concat l1 l2)
  (if (empty? l1)
    l2
    (concat (first l1) 
            (concat (rest l1)) l2)))

(trace concat)

;; count-elements: count the number of elements in a tree (a nested list)
(define (count-elements tree)
  (void)) ; recursive case

;; repeat: create a list of n copies of x
(define (repeat n x)
  (void))

;; reverse: reverse the elements of a list
(define (reverse lst)
  (void))

#|-----------------------------------------------------------------------------
;; Generative recursion

- what is generative recursion, and how does it differ from structural 
  recursion?
-----------------------------------------------------------------------------|#

(define (gcd m n)
  (cond
    [(= m 0) n]
    [(= n 0) m]
    [else (gcd n (remainder m n))]))
