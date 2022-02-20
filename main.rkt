; André Caetano Vidal       201665010AC
; Bernardo Souza Abreu Cruz 201635019

#lang racket

(require "irefs.rkt" "classes.rkt" "examples.rkt")

; ------------------------- A Linguagem CLASSES -------------------------

#|

  Um programa na linguagem CLASSES é uma sequência de declarações de classes seguido por uma expressão
  a ser executada. Uma declaração de classe possui um nome, nome da superclasse, zero ou mais declarações
  de campos e zero ou mais declarações de métodos. Uma declaração de método possui um nome, uma lista
  de parâmetros e um corpo. A linguagem foi implementada tendo base na linguagem IREF e no capítulo 9
  do livro Essentials of Programming Languages.

|#

; Especificação do comportamento das expressões
(define (value-of exp Δ)
  (let ([type (car exp)])
    (cond [(equal? type 'lit) (cadr exp)]

          [(equal? type 'var) (define v (cadr exp))
                              (if (thunk? v) (value-of (thunk-exp v) (thunk-env v))
                                  (deref (apply-env Δ v)))]

          [(equal? type '-) (- (value-of (cadr exp) Δ) (value-of (caddr exp) Δ))]

          [(equal? type 'zero?) (= (value-of (cadr exp) Δ) 0)]

          [(equal? type 'let) (value-of (cadddr exp)
                                        (extend-env (cadr exp) (newref (value-of (caddr exp) Δ)) Δ))]

          [(equal? type 'if) (if (value-of (cadr exp) Δ)
                                 (value-of (caddr exp) Δ) (value-of (cadddr exp) Δ))]

          [(equal? type 'proc) (proc-val (cadr exp) (caddr exp) Δ extend-env)]

          [(equal? type 'letrec) (value-of (car (cddddr exp))
                                           (extend-env-rec (cadr exp) (caddr exp) (cadddr exp) Δ value-of))]

          [(equal? type 'set) (let ([v (value-of (caddr exp) Δ)])
                                (setref! (apply-env Δ (cadr exp)) v)
                                v)]

          [(equal? type 'begin) (foldl (lambda (e acc)
                                         (value-of e Δ))
                                       (value-of (cadr exp) Δ)
                                       (cddr exp))]

          ; Retorna o objeto aplicado no método atual.
          [(equal? type 'self) (apply-env Δ '%self)]

          ; A partir do objeto passado, procura o método e operação nas classe e superclasse relacionados.
          ; A partir disso, caso o método seja encontrado, ele é retornado e aplicado em cima do objeto atual.
          [(equal? type 'send) (let ([args (values-of-exps (cadddr exp) Δ value-of)]
                                     [obj (value-of (cadr exp) Δ)])
                                 (apply-method
                                  (find-method
                                   (object-class-name obj)
                                   (caddr exp))
                                  obj
                                  args
                                  value-of))]

          ; Permite encontrar e executar algum método da superclasse a partir do objeto atual.
          [(equal? type 'super) (let ([args (values-of-exps (caddr exp) Δ value-of)]
                                      [obj (apply-env Δ '%self)])
                                  (apply-method
                                   (find-method (apply-env Δ '%super) (cadr exp))
                                   obj
                                   args
                                   value-of))]

          ; Utilizamos para criar um objeto novo de uma classe específica.
          ; Utilizado nas criações de instâncias principalmente no arquivo classes.
          [(equal? type 'new) (let ([args (values-of-exps (caddr exp) Δ value-of)]
                                    [obj (new-object (cadr exp))])
                                (apply-method
                                 (find-method (object-class-name obj) 'initialize)
                                 obj
                                 args
                                 value-of)
                                obj)]

          [else (error "operação não implementada")])))

; Especificação do comportamento do programa
(define value-of-program
  (lambda (pgm)
    (empty-store)
    (let ([class-decls (car pgm)]
          [body (cadr pgm)])
      (initialize-class-env! class-decls)
      (value-of body init-env))))

; Execução dos exemplos
(value-of-program example1)