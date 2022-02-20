; André Caetano Vidal       201665010AC
; Bernardo Souza Abreu Cruz 201635019

#lang racket

(provide example1)

; ----------------------------- Exemplos -----------------------------

#|

Exemplo 1

class c1 extends object
  method initialize () 1
  method m1 () send self m2()
  method m2 () 13
class c2 extends c1
  method m1 () 22
  method m2 () 23
  method m3 () super m1 ()
class c2 extends c2
  method m1 () 32
  method m2 () 33
let o3 = new c3()
  in send o3 m3()

|#

(define example1 '(((class c1 object () ((method initialize () (lit 1))
                                         (method m1 () (send (self) m2()))
                                         (method m2 () (lit 13))))
                    (class c2 c1 () ((method m1 () (lit 22))
                                     (method m2 () (lit 23))
                                     (method m3 () (super m1()))))
                    (class c3 c2 () ((method m1 () (lit 32))
                                     (method m2 () (lit 33)))))
                   (let o3 (new c3 ())
                     (send (var o3) m3 ()))))
