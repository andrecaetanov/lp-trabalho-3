#lang racket

; André Caetano Vidal       201665010AC
; Bernardo Souza Abreu Cruz 201635019

#| A Linguagem CLASSES

|#

;-------------------------------------------------------
#| Código base da linguagem IREF

Expressed values e Denoted values
  ExpVal = Int + Bool + Proc
  DenVal = Ref(ExpVal)

Operações para locations e referências
  newref :: ExpVal -> Ref
  deref  :: Ref -> ExpVal
  setref :: Ref x ExpVal -> ???

Notação do Estado
  σ        => denota um estado (memória, ou store)
  [l = v]σ => um estado que é igual a σ, exceto que no location l tem o valor v
  σ(l)     => valor no estado σ referenciado pelo location l

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

#| Environment

Env = Var -> Value

empty-env  :: Env
extend-env :: Var x Value x Env -> Env
apply-env  :: Env x Var -> Value

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

(struct thunk (env exp))

;-------------------------------------------------------
; Implementação da linguagem CLASSES

; 9.4.1 Objects

; the-class-env :: ClassEnv
(struct object (class-name fields))

(define new-object
  (lambda (class-name)
    (object
     class-name
     (map
      (lambda (field-name)
        (newref (list 'unitialized-field field-name)))
      (class-field-names (lookup-class class-name))))))

; 9.4.2 Methods

(struct method (vars body super-name field-names))

(define apply-method
  (lambda (m self args)
    (when (method? m)
      (let ([vars (method-vars m)]
            [body (method-body m)]
            [super-name (method-super-name m)]
            [field-names (method-field-names m)])
        (value-of body
                  (extend-env* vars (map newref args)
                               (extend-env-with-self-and-super
                                self super-name
                                (extend-env field-names (object-fields self)
                                            (empty-env)))))))))

; 9.4.3 Classes and Class Environment

(define the-class-env '())

(define add-to-class-env!
  (lambda (class-name class)
    (set! the-class-env
          (cons
           (list class-name class)
           the-class-env))))

(define lookup-class
  (lambda (name)
    (let ((maybe-pair (assq name the-class-env)))
      (if maybe-pair (cadr maybe-pair)
          (error (string-append "Unknown class " (symbol->string name)))))))

(struct class (name super-name field-names method-env))

(define initialize-class-env!
  (lambda (c-decls)
    (set! the-class-env
          (list
           (list 'object (class #f #f '() '()))))
    (for-each initialize-class-decl! c-decls)))

(define initialize-class-decl!
  (lambda (c-decl)
    (let ([c-name (cadr c-decl)]
          [s-name (car (caddr c-decl))]
          [f-names (cadr (caddr c-decl))]
          [m-decls (caddr (caddr c-decl))])
      (let ([f-names
             (append-field-names
              (class-field-names (lookup-class s-name))
              f-names)])
        (add-to-class-env!
         c-name
         (class c-name s-name f-names
           (merge-method-envs
            (class-method-env (lookup-class s-name))
            (method-decls->method-env
             m-decls s-name f-names))))))))

(define append-field-names
  (lambda (super-fields new-fields)
    (cond
      ((null? super-fields) new-fields)
      (else
       (cons
        (if (memq (car super-fields) new-fields)
            (fresh-identifier (car super-fields))
            (car super-fields))
        (append-field-names
         (cdr super-fields) new-fields))))))

; 9.4.4 Method Environments

(define find-method
  (lambda (c-name name)
    (let ([m-env (class-method-env (lookup-class c-name))])
      (let ([maybe-pair (assq name m-env)])
        (if (pair? maybe-pair) (cadr maybe-pair)
            (error (string-append "Method not found " (symbol->string name))))))))

(define method-decls->method-env
  (lambda (m-decls super-name field-names)
    (map
     (lambda (m-decl)
       (let ([method-name (car m-decl)]
             [vars (caddr m-decl)]
             [body (cadddr m-decl)])
         (list method-name
               (method vars body super-name field-names))))
     m-decls)))

(define merge-method-envs
  (lambda (super-m-env new-m-env)
    (append new-m-env super-m-env)))

; Métodos auxiliares
(define values-of-exps
  (lambda (exps env)
    (map
     (lambda (exp) (value-of exp env))
     exps)))

(define extend-env*
  (lambda (vars vals env)
    (if (null? vars)
        env
        (extend-env* (cdr vars)
                     (cdr vals)
                     (cons (cons (car vars) (car vals)) env)))))

(define (extend-env-with-self-and-super self super env)
  (lambda (svar)
    (case svar
      ((%self) self)
      ((%super) super)
      (else (apply-env env svar)))))

(define fresh-identifier
  (let ((sn 0))
    (lambda (identifier)
      (set! sn (+ sn 1))
      (string->symbol
       (string-append
        (symbol->string identifier)
        "%"
        (number->string sn))))))

; Especificação do comportamento das expressões
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

        ;-------------------------------------------------------
        [(equal? type 'self) (apply-env Δ '%self)]
        [(equal? type 'send) (let ([args (value-of (cadddr exp) Δ)]
                                   [obj (value-of (cadr exp) Δ)])
                               (apply-method
                                (find-method
                                 (object-class-name obj)
                                 (caddr exp))
                                obj
                                args))]
        [(equal? type 'super) (let ([args (values-of-exps (caddr exp) Δ)]
                                    [obj (apply-env Δ '%self)])
                                (apply-method
                                 (find-method (apply-env Δ '%super) (cadr exp))
                                 obj
                                 args))]
        [(equal? type 'new) (let ([args (values-of-exps (caddr exp) Δ)]
                                  [obj (new-object (cadr exp))])
                              (apply-method
                               (find-method (object-class-name obj) 'initialize)
                               obj
                               args)
                              obj)]

        [else (error "operação não implementada")]))

; Especificação do comportamento de programas
(define value-of-program
  (lambda (pgm)
    (empty-store)
    (let ([class-decls (car pgm)]
          [body (cadr pgm)])
      (initialize-class-env! class-decls)
      (value-of body the-class-env))))

;-------------------------------------------------------
; Exemplos
(define t1 '(((class c1 (object () ((method initialize () 1)
                                    (method m1 () (send self m2()))
                                    (method m2 () 13))))
              (class c2 (c1 () ((method m1 () 22)
                                (method m2 () 23)
                                (method m3 () (super m1())))))
              (class c3 (c2 () ((method m1 () 32)
                                (method m2 () 33)))))
             (let o3 (new c3 ())
               (send (var o3) m3 ()))))

(value-of-program t1)
