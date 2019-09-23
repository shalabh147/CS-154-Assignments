#lang racket
(require racket/struct)
(require "defs.rkt")
(require "examples.rkt")
(require "model-interpreter.rkt")



(displayln "Program 11********************************\n")
;
(displayln prog2)
;
(displayln "\n\nProgram evaluation ********************************\n")

(eval-program prog2)

;;and so on 