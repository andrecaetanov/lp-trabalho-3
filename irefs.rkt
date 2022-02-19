#lang racket

#| IREF: Uma Linguagem com Referências Implícitas

; Expressed values e Denoted values
ExpVal = Int + Bool + Proc
DenVal = Ref(ExpVal)
 
; Operações para locations e referências
  newref :: ExpVal -> Ref
  deref :: Ref -> ExpVal
  setref :: Ref x ExpVal -> ???

; Sintaxe
Prog -> (prog Expr)
Expr -> (lit Val) | (var Var) | (zero? Expr) | (dif Expr Expr)
     |  (if Expr Expr Expr) | (let Var Expr Expr)
     | (proc Var Expr) | (call Expr Expr)
     | (letrec Name Var Expr Expr)
     | (set Var Expr)
     | (begin Expr Expr*)

; Exemplo
1)
let g = let count = 0
        in proc(dummy)
            begin
             set count = -(count, -1));
             count
            end
in let a = (g 11)
   in let b = (g 11)
      in -(a,b)
_____________________________________________________
(let g (let count (lit 0)
        (proc dummy (begin (set count (dif (var count) (lit -1)))
                           (var count))))
 (let a (call (var g) (lit 11))
  (let b (call (var g) (lit 11))
   (dif (var a) (var b)))))


2)
let x = newref(newref(0))
in begin
    setref(deref(x), 11);
    deref(deref(x))
   end
______________________________________________________
Não tem equivalente em IREF


; Notação do Estado

σ => denota um estado (memória, ou store)
[l = v]σ => um estado que é igual a σ, exceto que no location l tem o valor v
σ(l) => valor no estado σ referenciado pelo location l

|#
; Representando um estado como um par: próximo endereço e um vetor
(define TAM 100) ; tamanho da memória
(define σ (cons 0 (make-vector TAM)))

;empty-store
(define (empty-store) (set! σ (cons 0 (cdr σ))))

;newref :: ExpVal -> Ref
(define (newref v)
  (define addr (car σ))
  (define mem (cdr σ))
  (vector-set! mem addr v)
  (set! σ (cons (add1 addr) mem))
  addr)

; deref :: Ref -> ExpVal
(define (deref addr)
  (if (< addr (car σ))
         (vector-ref (cdr σ) addr)
         (error "invalid location")))
; setref! :: Ref x ExpVal -> ?
(define (setref! addr v)
  (if (< addr (car σ))
      (vector-set! (cdr σ) addr v)
      (error "invalid location")))

#|
; Environment
Env = Var -> Value

empty-env :: Env
extend-env :: Var x Value x Env -> Env
apply-env :: Env x Var -> Value
    
; Notação
  Δ => Environment
  [] => empty-env
  [var=val]Δ => (extend-env var val Δ)
  [var1=val1, var2=val2]Δ => abreviação para [var1=val1]([var2=val2]Δ)
  [var1=val1, var2=val2, ..., varn=valn] => abreviação para [var1=val1,...,varn=valn][]
  Δ⟦var⟧  => (apply-env Δ var)

  |[name = var / body]Δ| = (extend-env-rec name var body Δ) 
|#
(define empty-env
  (lambda (var)
    (error "No bind")))

(define (extend-env var value env)
  (lambda (svar)
    (if (equal? svar var) value
        (apply-env env svar))))

(define (extend-env-rec name var body env)
  (lambda (svar)
    (if (equal? svar name)
        (newref (proc-val var body (extend-env-rec name var body env)))
        (apply-env env svar))))

(define (apply-env env var)
  (env var))

(define init-env empty-env)

#|
; Definição dos Valores

(apply-proc (procedure var body Δ) val) = (value-of body [var=l]Δ [l=val]σ)



; Passagem de Parâmetros
   (call fun param)

2) 

                Δ,σ ⊢ e₁ = (f, σ₁)     e₂ ∉ (var n) 
  _______________________________________________________________
          Δ,σ ⊢ (call e₁ e₂) = (apply-proc f thunk(e₂, Δ))


            Δ,σ ⊢ e₁ = (f, σ₁)      e₂ ∈ (var n)
  _______________________________________________________________
          Δ,σ ⊢ (call e₁ e₂) = (apply-proc-ref f Δ⟦n⟧)

3) 
2) call-by-reference

     Δ,σ ⊢ e₁ = (f, σ₁)  e₂ ∉ (var n)    Δ,σ₁ ⊢ e₂ = (v, σ₂)
  _______________________________________________________________
          Δ,σ ⊢ (call e₁ e₂) = (apply-proc f v)


            Δ,σ ⊢ e₁ = ((procedure param body Δ₁), σ₁)  e₂ ∈ (var n)   Δ,σ₁ ⊢ e₂ = (v, σ₂)     (apply-proc (procedure param body Δ₁) v) = (v₁, σ₂)
  _____________________________________________________________________________________________________________________________________________________
                                             Δ,σ ⊢ (call e₁ e₂)  = (v₁, [Δ⟦n⟧ = σ₂(Δ₁⟦param⟧)]σ₂



f(x)

x -> 5   .... 7
...



(f a)

a -> 5 .....   7

|#
; call-by-value
; proc-val :: Var x Expr x Env -> Proc
#;(define (proc-val var exp Δ)
  (lambda (val)
    (value-of exp (extend-env var (newref val) Δ))))

; apply-proc :: Proc x ExpVal -> ExpVal  
#;(define (apply-proc proc val)
  (proc val))


; call-by-reference
(define (proc-val var exp Δ)
  (lambda (val flag)
    (if flag (value-of exp (extend-env var (newref val) Δ))
        (value-of exp (extend-env var val Δ)))))

(define (apply-proc proc val)
  (proc val #t))

(define (apply-proc-ref proc val)
  (proc val #f))


#|
1) Passagem de parâmetro Natural
   DenVal = ExpVal
      LET, LETREC, PROC

2) Passagem de parâmetro por valor (call-by-value)
      EREF, IREF

  DenVal = Ref(ExpVal)
  Criar uma nova referência para o valor passado

3) Passagem de parâmetro por referência (call-by-reference)
      DenVal = Ref(ExpVal)

4) Passagem de parâmetro por resultado (call-by-value-result)
      DenVal = Ref(ExpVal)


5) Passagem de parâmetro por nome (call-by-name e call-by-need) lazy evaluation ; avaliação preguiçosa
      DenVal = Ref(ExpVal + Thunk)     
            



; Exemplo
1)
let p = proc (x) set x = 4
in let a = 3
   in begin (p a); a end

a e x são locais distintos
a -> |4| <- x

2)
let f = proc (x) set x = 44
in let g = proc (y) (f y)
   in let z = 55
      in begin (g z); z end

z -> |44| <- y
      ^
      x

3)
let swap = proc (x) proc(y)
            let temp = x
            in begin
                set x = y;
                set y = temp
               end
in let a = 33
   in let b = 44
      in begin
          ((swap a) b);
          -(a,b)
         end

a -> |44| <- x
b -> |33| <- y
temp -> |33|

4)
letrec loop(x) = loop(-(x,-1))
in let f = proc (z) 7
   in (f (loop 0))

|#

(struct thunk (env exp))

(define p1 '(let p (proc x (set x (lit 4)))
                (let a (lit 3)
                  (begin (call (var p) (var a)) (var a)))))

(define p2 '(let f (proc x (set x (lit 44)))
              (let g (proc y (call (var f) (var y)))
                (let z (lit 55)
                  (begin (call (var g) (var z)) (var z))))))

(define p3 '(let swap (proc x (proc y (let temp (var x)
                                        (begin (set x (var y))
                                               (set y (var temp))))))
              (let a (lit 33)
                (let b (lit 44)
                  (begin (call (call (var swap) (var a)) (var b))
                         (dif (var a) (var b)))))))

(define p4 '(letrec loop x (call (var loop) (dif (var x) (lit -1)))
                    (let f (proc x (lit 7))
                      (call (var f) (call (var loop) (lit 0))))))
#|
; Especificação do comportamento das expressões
      
               Δ,σ ⊢ e = (v,σ₁)
1) ________________________________________
       Δ,σ ⊢ (set n e) = (v, [Δ⟦n⟧ = v]σ₁) 

2) ____________________________________
        Δ,σ ⊢ (var x) = σ(Δ⟦x⟧)

     Δ,σ ⊢ e₁ = (v₁, σ₁)   l ∉ dom(σ₁)  [n = l]Δ,[l = v₁]σ₁ ⊢ e₂ = (v₂,σ₂)
3) _____________________________________________________________________________
                         Δ,σ ⊢ (let n e₁ e₂) = (v₂,σ₂)


|#
(define (value-of exp Δ)
  (define type (car exp))
  (cond [(equal? type 'lit) (cadr exp)]
        ; call-by-value e call-by-reference
        ;[(equal? type 'var) (deref (apply-env Δ (cadr exp)))]
        ; call-by-name
        [(equal? type 'var) (define v (cadr exp))
                            (if (thunk? v) (value-of (thunk-exp v) (thunk-env v))
                                (deref (apply-env Δ v)))]
        [(equal? type 'dif) (- (value-of (cadr exp) Δ) (value-of (caddr exp) Δ))]
        [(equal? type 'zero?) (= (value-of (cadr exp) Δ) 0)]
        [(equal? type 'let) (value-of (cadddr exp) (extend-env (cadr exp) (newref (value-of (caddr exp) Δ)) Δ))]
        [(equal? type 'if) (if (value-of (cadr exp) Δ)
                               (value-of (caddr exp) Δ) (value-of (cadddr exp) Δ))]
        [(equal? type 'proc) (proc-val (cadr exp) (caddr exp) Δ)]
        ; call-by-value
        ;[(equal? type 'call) (apply-proc (value-of (cadr exp) Δ) (value-of (caddr exp) Δ))]
        ; call-by-reference
        #;[(equal? type 'call) (if (equal? (car (caddr exp)) 'var)
                                 (apply-proc-ref (value-of (cadr exp) Δ) (apply-env Δ (cadr (caddr exp))))
                                 (apply-proc (value-of (cadr exp) Δ) (value-of (caddr exp) Δ)))]
        ;call-by-name
        [(equal? type 'call) (if (equal? (car (caddr exp)) 'var)
                                 (apply-proc-ref (value-of (cadr exp) Δ) (apply-env Δ (cadr (caddr exp))))
                                 (apply-proc (value-of (cadr exp) Δ) (thunk Δ (caddr exp))))]
        
        [(equal? type 'letrec) (value-of (car (cddddr exp)) (extend-env-rec (cadr exp) (caddr exp) (cadddr exp) Δ))]

        [(equal? type 'set) (let ([v (value-of (caddr exp) Δ)])
                              (setref! (apply-env Δ (cadr exp)) v)
                              v)]
        
        [(equal? type 'begin) (foldl (lambda (e acc)
                                       (value-of e Δ))
                                     (value-of (cadr exp) Δ)
                                     (cddr exp))]
        
        [else (error "operação não implementada")]))


; Especificação do comportamento de programas
(define (value-of-program prog)
  (empty-store)
  (value-of (cadr prog) init-env))

; Exemplos de expressões IREF
#;(define ex1 '(let g (let count (lit 0)
                      (proc dummy (begin (set count (dif (var count) (lit -1)))
                                         (var count))))
               (let a (call (var g) (lit 11))
                 (let b (call (var g) (lit 11))
                   (dif (var a) (var b))))))

;(value-of ex1 init-env)

#;(define ex2 '(program
              (letrec fun x (if (zero? (var x)) (lit 0)
                                (dif (var x)
                                     (dif (lit 0)
                                          (call (var fun) (dif (var x) (lit 1))))))
                      (call (var fun) (lit 3)))))

;(value-of-program ex2)

#|
; Exemplo
let p = proc (x) set x = 4
in let a = 3
   in begin (p a); a end

(define prog '(let p (proc x (set x (lit 4)))
                (let a (lit 3)
                  (begin (call (var p) (var a)) (var a)))))
|#

(value-of p1 init-env)
(value-of p2 init-env)
(value-of p3 init-env)
(value-of p4 init-env)
