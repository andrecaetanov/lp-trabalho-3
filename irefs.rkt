; André Caetano Vidal       201665010AC
; Bernardo Souza Abreu Cruz 201635019

#lang racket

(provide empty-store newref deref setref!
         empty-env init-env apply-env extend-env extend-env-rec
         proc-val apply-proc apply-proc-ref (struct-out thunk))

; --------- IREF: Uma Linguagem com Referências Implícitas ---------

#|

Notação do Estado
  σ        => denota um estado (memória, ou store)
  [l = v]σ => um estado que é igual a σ, exceto que no location l tem o valor v
  σ(l)     => valor no estado σ referenciado pelo location l

Expressed values e Denoted values
  ExpVal = Int + Bool + Proc
  DenVal = Ref(ExpVal)

|#

(define TAM 100)
(define σ (cons 0 (make-vector TAM)))

(define (empty-store) (set! σ (cons 0 (cdr σ))))

(define (newref v)
  (define addr (car σ))
  (define mem (cdr σ))
  (vector-set! mem addr v)
  (set! σ (cons (add1 addr) mem))
  addr)

(define (deref addr)
  (if (< addr (car σ))
      (vector-ref (cdr σ) addr)
      (error "invalid location")))

(define (setref! addr v)
  (if (< addr (car σ))
      (vector-set! (cdr σ) addr v)
      (error "invalid location")))

; -------------------------- Environtment --------------------------

#|

Env = Var -> Value

Notação
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

(define init-env empty-env)

(define (apply-env env var)
  (env var))

(define (extend-env var value env)
  (lambda (svar)
    (if (equal? svar var) value
        (apply-env env svar))))

(define (extend-env-rec name var body env value-of)
  (lambda (svar)
    (if (equal? svar name)
        (newref (proc-val var body (extend-env-rec name var body env)))
        (apply-env env svar))))

; --------------------------- Procedure ---------------------------

(define (proc-val var exp Δ value-of)
  (lambda (val flag)
    (if flag (value-of exp (extend-env var (newref val) Δ))
        (value-of exp (extend-env var val Δ)))))

(define (apply-proc proc val)
  (proc val #t))

(define (apply-proc-ref proc val)
  (proc val #f))

(struct thunk (env exp))