#lang racket
(require racket/struct)
(provide (all-defined-out))
(require "defs.rkt")
(require "examples.rkt")

;;Global definitions. A counter that tracks the framenumber
(define framenumber 0)

;The stack and its operations. I have decided to make the stack a global.
(define stack '())
(define (push frame) (set! stack (cons frame stack)))
(define (pop) (if (null? stack) (error "Empty stack")
                        (set! stack (cdr stack))))
(define (top) (if (null? stack) (error "Empty stack")
                        (car stack)))


;createframe creates a new frame. It gets its number from
;;framenumber and as a side effect increases framenumber
(define (createframe hashtable parent) ;hastable gives the initial bindings
   (begin  (set! framenumber (+ 1 framenumber)) (frame (- framenumber 1) hashtable parent)))

;This creates the global frame, which, in our case, contains
;empty bindings.
(push (createframe (make-hash '()) (emptyframe)))

;This interprets a program. It uses the following function processdef.
(define (eval-program prog)
         (match prog
           [(pgm deflist) (begin (map (lambda (x) (processdef x (top))) deflist)
                                 (return-value-of-main (top)))])
                            )         ;;added all definitions in the topmost frame

;;processdef describes how each definition is processed and added to
;;the frame fr.
(define (processdef defn fr)
  (match defn    
    [(def v/f exp) (hash-ref! (frame-bindings fr) v/f (eval-exp exp))])
                        )              ;;adds definition in the given frame

;; We have said that the result of the program is the value of
;; the symbol main. main must be a defined symbol in each program.
(define (return-value-of-main frame)
  (hash-ref! (frame-bindings frame) 'main "main not found"))

;; The expression evaluator is the heart of the interpreter.
;; It will use the functions below
(define (eval-exp exp)
  (cond [(symbol? exp) (find-frame exp (top))]
        [(boolean? exp) exp]
        [(number? exp) exp]
        [(list? exp) exp]
        [(string? exp) exp]
        [else (match exp
                [(uexp op exp1) (op (eval-exp exp1))]
                [(bexp op exp1 exp2) (op (eval-exp exp1) (eval-exp exp2))]
                [(lam var expression) (closure (lam var expression) (top))]
                [(app exp1 explist)
                 (let* ([evaled (eval-exp exp1)]
                        [c-l (closure-lambda evaled)]
                        [arg-list (lam-varlist c-l)]
                        [parent-f (closure-frame evaled)]
                        [fr (createframe (make-hash '()) parent-f)]
                        [inside (lam-exp c-l)])
                   (match inside [(defexp lst exp2) (begin
                                                   (map (lambda (x) (hash-ref! (frame-bindings fr) (list-ref arg-list x) (eval-exp (list-ref explist x))))
                                                   (build-list (length arg-list) values))
                                                   (push fr)
                                                    (map (lambda (x) (processdef x (top))) lst)
                                                    (let ([ans (eval-exp exp2)])
                                                 (pop) ans))]
                   [_ (begin (map (lambda (x) (hash-ref! (frame-bindings fr) (list-ref arg-list x) (eval-exp (list-ref explist x))))
                               (build-list (length arg-list) values))
                          (push fr)
                          (let ([ans (eval-exp (lam-exp (closure-lambda evaled)))])
                            (pop) ans))]))]
                [(iff cond exp1 exp2) (if (eval-exp cond) (eval-exp exp1) (eval-exp exp2))]
                [(lett defexp exp) (process-lett defexp exp)]
                [(lets defexp exp) (display "Entered")                                     
                                     (process-lets defexp exp)]
;                [(lets defexp exp) (cond[(null? deflist) ]
;                                        [else (process-lett
;                                                (eval-exp (lets (process
                [(sett exp1 exp2) (hash-set! (frame-bindings (search exp1 (top))) exp1 (eval-exp exp2))]
                [(beginexp explist) (process-beginexp explist)]
                [(defexp lst exp2) (begin (map (lambda (x) (processdef x (top))) lst) (eval-exp exp2))]
               ; [(lett deflist exp) (process-lets deflist exp)]
               ; '()and so on, fill in these'()
                [(debugexp) (begin
                              (print-current-environment (top))
                              )])]))

(define (find-frame exp f)
  (define e (search exp f))
  (if(emptyframe? e) (error "Symbol not found")
     (hash-ref (frame-bindings e) exp)))


;;An auxilliary function that processes a begin expression
(define (process-beginexp explist)
  (match explist [(cons x l) (cond[(null? l) (eval-exp x)]
                                   [else (begin (eval-exp x) (process-beginexp l))])]))
                  
   

;;An auxilliary function that processes a let expression.
;;The let definitions are in deflist, and the let body is exp.
(define (process-lett deflist exp)
    (let ([fr (createframe (make-hash '()) (top))])
      (begin
        (map (lambda (x)
               (match x [(def m binding) (hash-ref! (frame-bindings fr) m (eval-exp binding))]))
             deflist)
        (push fr)
        (let ([ans (eval-exp exp)])
        (pop)
          ans))))

(define (process-lets deflist exp)
 ; (displayln "Hello")
  ;(displayln (top))
    (if(null? deflist) (begin
                         (let([ans (eval-exp exp)])
                           (pop)
                           ans))
       (begin (match (car deflist) [(def m binding) (let ([fr (createframe (make-hash '()) (top))])
                                                      (begin (hash-ref! (frame-bindings fr) m (eval-exp binding)) (push fr)))])
              (process-lets (cdr deflist) exp))))


           

;;Prints the current environment running through a chain of frames.
;;Note that my struct definitions are such that if fr is a frame,
;;then (displayln fr) will print the frame in the format that I have
;;shown. 
(define (print-current-environment fr)
  (if (emptyframe? fr) (displayln "@@@@@@@@@@@@@@@@@@@@@@@@")
      (begin (displayln "@@@@@@@@@@@@@@@@@@@@@@@@")
            (displayln fr)
            (print-current-environment (frame-parent fr)))))

;;Search for the symbol sym in an environment that starts with the frame
;;fr. We shall make search return either the  emptyframe
;;or the frame containing the symbol (note, not the value of the
;;symbol itself.

(define (search sym fr)
 (cond[(emptyframe? fr) fr]
      [else (cond[(hash-has-key? (frame-bindings fr) sym) fr]
                 [else (search sym (frame-parent fr))])]))



(define prog11
   (pgm (list
        [def  'a 1]
	[def  'b 2]
        [def 'main (beginexp (list (debugexp) (bexp + 'a 'b)))])))

(define prog12
   (pgm (list
        [def  'a 1]
	[def  'b 2]
	[def 'c (lam '() (beginexp (list (debugexp) (bexp + 'a 'b))))]	
        [def 'main (app 'c '())])))

(define prog5
 (pgm (list
       [def 'make-account
         (lam (list 'balance) 
              (lam (list 'amount)
                   (iff (bexp >= 'balance 'amount) 
                        (beginexp
                         (list 
                          (sett 'balance
                                (bexp - 'balance 'amount))
                          'balance))
                       "Insufficient funds")))]
       [def 'my-account (beginexp
                          (list
                           (debugexp)
                           (app 'make-account (list 50))))]
       [def 'your-account (app 'make-account (list 1000))]
       [def 'main1 (app 'my-account (list 20))]
       [def 'main (beginexp
                          (list
                           (debugexp)
                           (app 'your-account (list 20))))])))