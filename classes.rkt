; André Caetano Vidal       201665010AC
; Bernardo Souza Abreu Cruz 201635019

#lang racket

(require "irefs.rkt")
(provide (struct-out object) new-object apply-method initialize-class-env! find-method values-of-exps)

; ----------------------------- Structs -----------------------------

(struct class (super-name field-names method-env))
(struct method (vars body super-name field-names))
(struct object (class-name fields))

; -------------------------- 9.4.1 Objects --------------------------

(define new-object
  (lambda (class-name)
    (object
     class-name
     (map
      (lambda (field-name)
        (newref (list 'unitialized-field field-name)))
      (class-field-names (lookup-class class-name))))))

; -------------------------- 9.4.2 Methods --------------------------

(define apply-method
  (lambda (m self args value-of)
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
                                            empty-env))))))))

; --------------- 9.4.3 Classes and Class Environment ---------------

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

(define initialize-class-env!
  (lambda (c-decls)
    (set! the-class-env
          (list
           (list 'object (class #f '() '()))))
    (for-each initialize-class-decl! c-decls)))

(define initialize-class-decl!
  (lambda (c-decl)
    (let ([c-name (cadr c-decl)]
          [s-name (caddr c-decl)]
          [f-names (cadddr c-decl)]
          [m-decls (car (cddddr c-decl))])
      (let ([f-names
             (append-field-names
              (class-field-names (lookup-class s-name))
              f-names)])
        (add-to-class-env!
         c-name
         (class s-name f-names
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

; -------------------- 9.4.4 Method Environments --------------------

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
       (let ([method-name (cadr m-decl)]
             [vars (caddr m-decl)]
             [body (cadddr m-decl)])
         (list method-name
               (method vars body super-name field-names))))
     m-decls)))

(define merge-method-envs
  (lambda (super-m-env new-m-env)
    (append new-m-env super-m-env)))

; ----------------------- Métodos auxiliares -----------------------

(define values-of-exps
  (lambda (exps env value-of)
    (map
     (lambda (exp) (value-of exp env))
     exps)))

(define fresh-identifier
  (let ((sn 0))
    (lambda (identifier)
      (set! sn (+ sn 1))
      (string->symbol
       (string-append
        (symbol->string identifier)
        "%"
        (number->string sn))))))

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