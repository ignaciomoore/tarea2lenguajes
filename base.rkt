#lang play

;################################ Interprete visto en clases ###########################

#|
<expr> ::= (num <num>)
         | (add <expr> <expr>)
         | (sub <expr> <expr>)
         | (if0 <expr> <expr> <expr>)
         | (id <id>)
         | (fun <sym> <expr>)
         | (app <expr> <expr>)
|#
(deftype Expr
  (num n)
  (add l r)
  (sub l r)
  (if0 c t f)
  (id x)
  (fun arg body)
  (app f-name f-arg))


;; parse :: s-expr -> Expr
;; converts s-exprs into Exprs where
#|
<s-expr> ::= <num>
           | <sym>
           | (list '+  <s-expr> <s-expr>)
           | (list '-  <s-expr> <s-expr>)
           | (list 'if0  <s-expr> <s-expr> <s-expr>)
           | (list 'fun (list <sym>) <s-expr>)
           | (list <s-expr> <s-expr>)
           | (list 'with (list <sym> <s-expr>) <s-expr>)
|#
(define (parse s-expr)
  (match s-expr
    [ n #:when (number? n) (num n)]
    [ x #:when (symbol? x) (id x)]
    [(list '+ l r) (add (parse l) (parse r))]
    [(list '- l r) (sub (parse l) (parse r))]
    [(list 'if0 c t f) (if0 (parse c) (parse t) (parse f))]
    [(list 'fun (list x) b) (fun x (parse b))]
    [(list f a) (app (parse f) (parse a))]    
    [(list 'with (list x e) b) #:when (symbol? x)
         (app (fun x (parse b)) (parse e))]))


;; Abstract Dada Type (ADT) for handling environments 
;; empty-env  :: Env
;; extend-env :: Symbol Value Env -> Env
;; env-lookup :: Symbol Env -> Value

;; <env> ::= mtEnv
;;         | (aEnv <id> <value> <env>)
(deftype Env
  (mtEnv)
  (aEnv id val env))

(define empty-env (mtEnv))
 
(define extend-env aEnv)
 
(define (env-lookup x env)
  (match env
    [(mtEnv) (error 'env-lookup "free identifier: ~a" x)]
    [(aEnv id val rest) (if (symbol=? id x)
                            val
                            (env-lookup x rest))]))


;; Values of expressions 
;; <value> ::= (numV <number>)
;;          |  (closureV <sym> <s-expr> <env>) 
(deftype Value
  (numV n)
  (closureV id body env))

;; Auxiliary functions handling numeric Values
(define (op-bin f n1 n2)
  (numV (f (numV-n n1) (numV-n n2))))

(define (op-un f n)
  (numV (f (numV-n n))))


;; eval :: Expr Env -> Value
;; evaluates an expression in a given
;; environment using static scoping 
(define (eval expr env)
  (match expr
    [(num n) (numV n)]
    [(fun id body) (closureV id body env)]
    [(id x) (env-lookup x env)]
    [(add l r) (op-bin + (eval l env) (eval r env))]
    [(sub l r) (op-bin - (eval l env) (eval r env))]
    [(if0 c t f) (if  (op-un zero? (eval c env))
                      (eval t env)
                      (eval f env))]
    [(app f e) (def (closureV the-arg the-body the-claus-env) (eval f env))
               (def the-ext-env (extend-env the-arg (eval e env) the-claus-env))
               (eval the-body the-ext-env)]))


;; run :: s-expr -> Value
(define (run prog)
  (eval (parse prog) (mtEnv)))





;################################ Definiciones ###########################

(deftype Type
  (TNum)
  (TFun Targ Tret)
  (TVar Symbol))

(deftype Constraint
  (Cnst T1 T2))

(deftype TEnv
  (mtTEnv)
  (anTEnv id Type env))

(define count 0)

(define (get-id)
  (begin
    (set! count (add1 count))
    count))

(define (reset)
  (set! count 0))

(define (prettyfy T)
  (match T
    [(TNum) "num"]
    [(TVar x) (string-append "(TVar "(number->string x) ")")]
    [(TFun T1 T2) (string-append "(TFun " (prettyfy T1) " " (prettyfy T2) ")")]))




;################################ Su cÃ³digo va aquÃ­ ###########################

;###### Ejercicio 1 #########

;## (a) ##

;; Construye un ambiente de tipos vacio.
;; emptyT-env :: TEnv
(define emptyT-env
  (mtTEnv)
)

;; Extiende un ambiente asociando un tipo a un identificador dado.
;; extendT-env :: Sym Type TEnv -> TEnv
(define (extendT-env x type environment)
  (anTEnv x type environment)
)

;; Dado un identificador y un ambiente de tipos, retorna el tipo asociado al identificador.
;; lookupT-env :: Sym TEnv -> TEnv
(define (lookupT-env x environment)
  (match environment
    [(mtTEnv) (error 'lookupT-Env "free identifier: ~a" x)]
    [(anTEnv id Type rest) (if (symbol=? id x)
			      Type
			      (lookupT-env x rest))]
  )
)

;## (b) ##

(define (typeof expr env)
  (void))

;###### Ejercicio 2 #########

;## (a) ##
  
(define (substitute from to _list)
  (void))

;## (b) ##

(define (occurs-in? tvar t)
  (void))

;## (c) ##

(define (unify _list)
  (void))

;###### Ejercicio 3 #########

;## (a) ##

(define (runType s-expr)
  (void))
