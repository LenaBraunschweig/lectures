#lang racket

#|-----------------------------------------------------------------------------
;; Adding Functions & Closures

We will add functions (as lambda expressions) and function applications to our
language. Our functions will have exactly one formal parameter each.

- interpreter tokenizes -> parses -> evaluates

E.g.,

- lambda definition: `(lambda (x) (+ x 1))`

- function application: `((lambda (x) (+ x 1)) 10)`
  - calls the function on the argument 10

Though our language will not support named functions a la Racket's `define`,
we can use `let` to bind identifiers to lambdas. E.g.,

  (let ([f (lambda (x) (+ x 1))])
    (f 10))
-----------------------------------------------------------------------------|#

;; Some test cases (what should they evaluate to?)
(define p1 '(lambda (x) (+ x 1)))

(define p2 '((lambda (x) (+ x 1)) 10))

(define p3 '(let ([f (lambda (x) (+ x 1))])
              (f 10)))

;; p4-p7 for testing closures
(define p4 '(let ([x 10])
              (lambda (y) (+ x y))))

(define p5 '(let ([x 10])
              ((lambda (y) (+ x y)) 20)))

(define p6 '(let ([f (let ([x 10])
                       (lambda (y) (+ x y)))])
              (let ([x 20])
                (f x))))

(define p7 '(let ([f (let ([x 10])
                       (lambda (y) (+ x y)))])
              (f 20)))


;; integer value
(struct int-exp (val) #:transparent)

;; arithmetic expression
(struct arith-exp (op lhs rhs) #:transparent)

;; variable
(struct var-exp (id) #:transparent)

;; let expression
(struct let-exp (ids vals body) #:transparent)

;; lambda expression
(struct lambda-exp (id body) #:transparent)

;; function application
(struct app-exp (lhs rhs) #:transparent)


;; Parser
(define (parse sexp)
  (match sexp
    ;; integer literal
    [(? integer?)
     (int-exp sexp)]

    ;; arithmetic expressions
    [(list '+ lhs rhs)
     (arith-exp "PLUS" (parse lhs) (parse rhs))] 
    [(list '* lhs rhs)
     (arith-exp "TIMES" (parse lhs) (parse rhs))]
    
    ;; identifiers (variables)
    [(? symbol?)
     (var-exp sexp)]

    ;; let expressions
    [(list 'let (list (list id val) ...) body)
     (let-exp id (map parse val) (parse body))]
    
    ;; lambda expressions
    [(list 'lambda (list id) body) 
      (lambda-exp id parse(body))] ;; the body itself (like (+ x 1)) needs to be parsed

    ;; function application
    [(list fn arg) 
      (app-exp (parse fn) (parse arg))]

    ;; basic error handling
    [_ (error (format "Can't parse: ~a" sexp))]))

;; acts like a lambda in the lexical environment (where the code was typed)
(struct closure (id body env) #:transparent) ;; captures the current environement, data structure created by the interpreter

;; Interpreter
(define (eval expr [env '()])
  (match expr
    ;; int literals
    [(int-exp val) val]

    ;; arithmetic expressions    
    [(arith-exp "PLUS" lhs rhs)
      (+ (eval lhs env) (eval rhs env))]
    [(arith-exp "TIMES" lhs rhs)
      (* (eval lhs env) (eval rhs env))]         

    ;; variable binding
    [(var-exp id)
      (let ([pair (assoc id env)])
        (if pair
            (cdr pair)
            (error (format "~a not bound!" id))))]

    ;; let expression with multiple variables
    [(let-exp (list id ...) (list val ...) body)
      (let ([vars (map cons id
                      (map (lambda (v)
                              (eval v env))
                            val))])
        (eval body (append vars env)))]

    ;; lambda expression
    [(lambda-exp id body)  ;; need to be able to save its environment so that let expressions will save
      (closure id body env)] ; captures current enviornment
    
    ;; function application
    [(app-exp lhs rhs) 
    ;; e.g. (f 10), (f (+ 1 2)), ((lambda (x) (+ x 5)))
    ;; left hand size needs to give us a lambda
      (let* ([clo (eval lhs env)] ;; lhs gives lambda, rhs gives us argument
             [arg (eval rhs env)])
        (eval (closure-body clo)
          ((cons (closure-id clo) arg) (closure-env clo))))] ;; accesses the body argument of the lambda-exp struct

    ;; basic error handling
    [_ (error (format "Can't evaluate: ~a" expr))]))


;; REPL
(define (repl)
  (let ([stx (parse (read))])
    (when stx
      (println (eval stx))
      (repl))))
