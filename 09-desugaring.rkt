#lang racket


#|-----------------------------------------------------------------------------
;; Desugaring (Syntax Transformation)

Our language now includes arithmetic expressions, variables, and closures.
We can (and will!) grow the language, but how should we go about building
support for new language constructs?

Option 1: modify the interpreter to recognize and implement new constructs

Option 2: restrict "core" language (simplest vocabulary) to a small set of features and ...?

---

Pros/Cons? (Discussion)
  - option 1 is faster, better for memory, and easy to test
  - option 2 is easier to implement and easy to scale
---

Parse / Desugar / Interpret workflow:

    Program (input language) => Parser => Syntax Tree / IR (input language)

    IR (input language) => Desugaring Pass(es) ... => IR (core language)

    IR (core language) => Interpreter / Eval

---

As an example of desugaring, we will add support for lambdas and function
applications that accept > 1 params/args. This will *not* require any
modifications to our interpreter, as we can rewrite such expressions using
the existing core language.

e.g., how might we desugar a lambda of more than 1 parameter?

    (lambda (x y z ...)
      body)

    (lambda (x)
      (lambda (y)
        (lambda(z)
          ... body)))

e.g., how might we desugar a function application with more than 1 argument?

    (f x y z ...)
    (((f x ) y) z)

- no new features, but instead new syntax
- acts as a relly simple foldr

-----------------------------------------------------------------------------|#

;; Some test cases
(define p1 '(lambda (x y z) (* x (+ y z))))

(define p2 '(f x y z))

(define p3 '((lambda (x y z) (* x (+ y z))) 2 3 4))


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
(struct app-exp (fn arg) #:transparent)


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
     (let-exp (map parse id) (map parse val) (parse body))]
    
    ;; lambda expressions -- modified for > 1 params
    [(list 'lambda (list ids ...) body)
     (lambda-exp ids (parse body))]

    ;; function application -- modified for > 1 args
    [(list f args ...)
     (app-exp (parse f) (map parse args))]

    ;; basic error handling
    [_ (error (format "Can't parse: ~a" sexp))]))


;; Desugar-er -- i.e., syntax transformer
;; all elements in the tree need to be desugared
(define (desugar exp)
  (match exp
    [(arith-exp op lhs rhs)
      (arith-exp op (desugar lhs) (desugar rhs))]
    [(let-exp ids vals body)
      (let-exp ids (map desugar vals) (desugar body))]
    [(lambda-exp ids body)
      (foldl (lambda (id body) (lambda-exp id body)) (desugar body) ids)]
    [(app-exp fn args)
      (foldl (lambda (id fn) (app-exp fn id) (desugar fn) (map desugar args)))]
    (_ exp)))


;; function value + closure
(struct fun-val (id body env) #:transparent)


;; Interpreter
(define (eval expr)
  (let eval-env ([expr expr]
                 [env '()])
    (match expr
      ;; int literal
      [(int-exp val) val]

      ;; arithmetic expressions
      [(arith-exp "PLUS" lhs rhs)
       (+ (eval-env lhs env) (eval-env rhs env))]
      [(arith-exp "TIMES" lhs rhs)
       (* (eval-env lhs env) (eval-env rhs env))]         
      
      ;; variable binding
      [(var-exp id)
       (let ([pair (assoc id env)])
         (if pair (cdr pair) (error (format "~a not bound!" id))))]

      ;; let expression
      [(let-exp (list (var-exp id) ...) (list val ...) body)
       (let ([vars (map cons id
                        (map (lambda (v) (eval-env v env)) val))])
         (eval-env body (append vars env)))]

      ;; lambda expression
      [(lambda-exp id body)
       (fun-val id body env)]
      
      ;; function application
      [(app-exp f arg)
       (match-let ([(fun-val id body clenv) (eval-env f env)]
                   [arg-val (eval-env arg env)])
         (eval-env body (cons (cons id arg-val) clenv)))]

      ;; basic error handling
      [_ (error (format "Can't evaluate: ~a" expr))])))


;; REPL
(define (repl)
  (let ([stx (desugar (parse (read)))])
    (when stx
      (println (eval stx))
      (repl))))