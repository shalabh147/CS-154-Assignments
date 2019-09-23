#lang racket/base

;; You can require more modules of your choice.
(require racket/list
         racket/string
         (prefix-in utils: "utils.rkt"))

(provide dictionary-closure)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;; Dictionary Closure                                                               ;;
;; ==================                                                               ;;
;;                                                                                  ;;
;; A choice of substitution can really trigger more substitutions by looking at the ;;
;; partially decrypted text - surely there will be some words which can be uniquely ;;
;; determined using the dictionary. It is prudent to concretize this "choice" as    ;;
;; this purely deterministic (involving absolutely no guess-work). In more          ;;
;; technical terms, this is called "maintaining arc-consistency" (look it up on     ;;
;; Wikipedia).                                                                      ;;
;;                                                                                  ;;
;; This function must utilise the dictionary and the cipher-word-list. Decrypt each ;;
;; word (`utils:decrypt`) and check if the dictionary has:                          ;;
;;                                                                                  ;;
;; 1. a unique completetion!                                                        ;;
;;    - Extend your key using the information with this match. Continue exploring   ;;
;;      the words under the extended key.                                           ;;
;; 2. many completions.                                                             ;;
;;    - Do nothing, just continue exploring the words under the same key. If none   ;;
;;      of the words fall into (1), return with the key that you have built so far. ;;
;; 3. no completions!                                                               ;;
;;    - Return `#f` (false), indicating that this partial key is wrong, and we must ;;
;;      revert to the original key.                                                 ;;
;;                                                                                  ;;
;; Returns either of:                                                               ;;
;; a. a (possibly) extended-key.                                                    ;;
;; b. `#f` if this key led to case (3) for some word.                               ;;
;;                                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define l utils:cipher-word-list)                   
(define (decrypter l k)
  (if(null? l) '()
  (cons (utils:decrypt k (car l)) (decrypter (cdr l) k))))       ;returns decrypted word-list according to present key

(define (dictionary-closure key)
 ; (define decrypted-list (decrypter l key))
  (dict-helper (decrypter l key)  utils:dictionary key)          ;helper-function which takes decrypted word list as argument and present key as arguments along with dictionary
 )





;(define empty-key (cons #\w (make-list 25 #\_)))

(define (dict-helper l2 dictionary key)
  (if(null? l2) key
  (if (uppercase? (car l2)) (dict-helper (cdr l2) dictionary key)
           (begin (displayln (car l2))                                ;if word-list is over return key that is present(word-list will end only when key is extended maximum)
  (let ([a (match (string->list (car l2)) dictionary 0 '() key)])     ;match function returns pair of words that match with decrypted-word and their count
  (cond[(= (cdr a) 0) #f]                                            
       [(= (cdr a) 1) (begin (displayln "------------------------------------------------") 
                             (if (not (utils:is-monoalphabetic? (rev-order (caar a)) key)) #f
                                 (dictionary-closure (utils:add-substitution (rev-order (caar a)) key ))))]        ;if a unique match is found then call dict-closure again with extended key
      [else (dict-helper (cdr l2) dictionary key)]))))))
(define (uppercase? str)
  (andmap (lambda (x) (char-upper-case? x)) (string->list str)))
(define (match sl l4 k l3 key)
  (if(= k 2) (cons l3 k)                                          ;once it exceeds 1 match return as no chance of improvement with this word
  (if(null? l4) (cons l3 k)
      ;if dictionary is over 
  (if(= (length sl) (length (string->list (car l4)))) (if (= (length (perfect-match sl (string->list (car l4)))) 0) (cons '() 2)
                                                          (cond[(no-capital (perfect-match sl (string->list (car l4)))) (if (and (utils:is-monoalphabetic? (rev-order (perfect-match sl (string->list (car l4)))) key) (locally-consistent (perfect-match sl (string->list (car l4))))) (match sl (cdr l4) (+ k 1) (cons (perfect-match sl (string->list (car l4))) l3) key) (match sl (cdr l4) k l3 key)  )]
                                                               [else (match sl (cdr l4) k l3 key)]))                                                         
     (match sl (cdr l4) k l3 key)))))



(define (perfect-match sl l4)
  (define zipped (zip sl l4))
  (define without-capital (check zipped))
  (remove-duplicates without-capital))

(define (no-capital q)
  (if(null? q) #t
     (if(char-upper-case? (caar q)) #f
        (no-capital (cdr q)))))

(define (check lst)
  (if(null? lst) '()
     (cond[(equal? (caar lst) (cdar lst)) (check (cdr lst))]
          [else (cons (car lst) (check (cdr lst)))])))

(define (zip l1 l2)
  (if(or (null? l1) (null? l2)) '()
     (cons (cons (car l1) (car l2)) (zip (cdr l1) (cdr l2)))))

(define empty-key (make-list 26 #\_))
(define (locally-consistent substitution)
  (not (or
        (check-duplicates (for/list ([subst-pair substitution])
                            (car subst-pair)))
        (check-duplicates (for/list ([subst-pair substitution])
                            (cdr subst-pair))))))

(define (rev-order l)
  (map (lambda (x) (cons (cdr x) (car x))) l)) 