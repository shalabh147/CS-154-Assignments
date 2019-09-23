#lang racket
;(require "list-comprehensions.rkt")
;; You can require more modules of your choice.
(require racket/list
         (prefix-in utils: "utils.rkt")
         (prefix-in stats: "statistics.rkt"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                     ;;
;; Strategies                                                                          ;;
;; ==========                                                                          ;;
;; For the purpose of this assignment, just the `etai` strategy is expected, since     ;;
;; we have dictionary-closure and secret-word-enumeration to leap-frog to the right    ;;
;; key. This technique would fail for harder keys which are arbitrary permutations of  ;;
;; the alphabet. We will be forced to explore many more strategies (along with         ;;
;; dictionary-closure of course).                                                      ;;
;;                                                                                     ;;
;; Strategies to guess substitutions for the key using statistical information about   ;;
;; - the English language from utils.rkt                                               ;;
;; - the cipher text      from statistics.rkt                                          ;;
;;                                                                                     ;;
;; Follow the function signature as indicated below. Deviations will make it           ;;
;; impossible for automatic grading of your submission.                                ;;
;; Moreover, we do not expect your strategies to require any more/different            ;;
;; arguments. Note that you recieve the key as argument, so you can at the very        ;;
;; least ensure that all the substitutions are monoalphabetic wrt this key.            ;;
;;                                                                                     ;;
;; Signature:                                                                          ;;
;; ```                                                                                 ;;
;; (define (my-fundoo-strategy key)                                                    ;;
;;   ;; Make use of `utils:ciphertext`, `utils:cipher-word-list`                       ;;
;;   ...)                                                                              ;;
;; ```                                                                                 ;;
;;                                                                                     ;;
;; Substitutions                                                                       ;;
;; -------------                                                                       ;;
;; In order to extend the key incrementally, we use `utils:add-substitution` to        ;;
;; extend a given key with a substitution.                                             ;;
;;                                                                                     ;;
;; A substitution is a list of pairs, each pair mapping a plaintext char to a          ;;
;; ciphertext char. For example, to extend the key with T -> a and O -> r              ;;
;; (simultaneously), we use the substitution:                                          ;;
;; ```                                                                                 ;;
;; (list (cons #\T #\a) (cons #\O #\r))                                                ;;
;; ```                                                                                 ;;
;; For a single substitution use a singleton list (containing just one pair).          ;;
;;                                                                                     ;;
;; **CAUTION**                                                                         ;;
;; -----------                                                                         ;;
;; 1. Note that add-substitution does not do sanity checks on the substitution and use ;;
;;    of `utils:is-monoalphabetic` is recommended to ensure that you don't             ;;
;;    inadvertently create invalid keys.                                               ;;
;; 2. You must provide a list called `compositions` in this module.                    ;;
;;                                                                                     ;;
;; See docs in "utils.rkt" for more information.                                       ;;
;;                                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; You must add "public" functions of this module to this list.
(provide etai
         ;; Some more suggested strategies:
         
         ;; common-words-double
         ;; bigrams
         ;; common-initial-letters
         ;; common-final-letters
         ;; common-words-triple
         ;; trigrams
         ;; common-double-letters
         ;; common-words-quadruple
         ;; quadgrams
         
         ;; lists of strategies
         composition)
(define empty-key (make-list 26 #\_))
;; A strategy that uses some statistical information to generate potential
;; substitutions for E, T, A and I.
;; Refer the assignment manual for tips on developing this strategy. You can
;; interact with our etai with the executable we provide.
(define (ret-first-ele l)
  (if(null? l) '()
     (cons (caar l) (ret-first-ele (cdr l)))))                                  ;;this function returns a list of car's of pairs in the list that is argument

(define (pref-order-for-t l1 l2 l3)
    (if(null? l1) l3
    (if(member (car (car l1)) l2) (pref-order-for-t (cdr l1) l2 (cons (car l1) l3))
       (pref-order-for-t (cdr l1) l2 l3))))

 (define (comparator x y)
   (< (cdr x) (cdr y)))



(define list-of-monograms (stats:cipher-monograms utils:ciphertext))
  (define list-most-frequent-five (reverse (list-tail (reverse list-of-monograms) 20)))
  (define single-letter-word-list (stats:cipher-common-words-single utils:cipher-word-list))
  (define most-frequent-neighbour-list (stats:cipher-unique-neighbourhood (stats:cipher-bigrams utils:cipher-word-list) 'both))
  (define pref-order (ret-first-ele (sort ( pref-order-for-t most-frequent-neighbour-list list-most-frequent-five '()) comparator)))      ;list of preference order for #\T

(define (etai key)
 (if(= (length single-letter-word-list) 0) (check-monoalphabetic key (remove-duplicates(append* ( create-list-of-sub pref-order))))
    (if(= (length single-letter-word-list) 1) (check-monoalphabetic key (append (remove-duplicates (append* (give-a-single (remove (string-ref (car single-letter-word-list) 0) pref-order)))) (remove-duplicates (append* (give-i-single (remove (string-ref (car single-letter-word-list) 0) pref-order)))))) 
     (check-monoalphabetic key (append (remove-duplicates (append* (a-first-ele (remove (string-ref (car single-letter-word-list) 0) (remove (string-ref (cadr single-letter-word-list) 0) pref-order))))) (remove-duplicates (append* (i-first-ele (remove (string-ref (car single-letter-word-list) 0) (remove (string-ref (cadr single-letter-word-list) 0) pref-order))))))))
))

(define (check-monoalphabetic k l)
  (if(null? l) '()
     (if(utils:is-monoalphabetic? (car l) k) (cons (car l) (check-monoalphabetic k (cdr l)))             ;;this function is present in case ther was some key given which wasnt empty.
        (check-monoalphabetic k (cdr l)))))


    (define (create-list-of-sub pref-order)
  (if(null? pref-order) '()
     (cons (some-function-to-create-substitution (car pref-order)) (create-list-of-sub (cdr pref-order)))))

(define (some-function-to-create-substitution ch)
  (map (lambda (z) (append (list (car z)) (list (cons #\T ch)) (cdr z))) (map (lambda (x) (zip '(#\E #\A #\I) x)) (permutations (remove ch list-most-frequent-five)))))

(define (zip l1 l2)
  (if(or (null? l1 ) (null? l2)) '()
     (cons (cons (car l1) (car l2)) (zip (cdr l1) (cdr l2)))))

(define (give-a-single pref-order)
  (if(null? pref-order) '()
     (cons (a-helper (car pref-order)) (give-a-single (cdr pref-order)))))

(define (a-helper ch)
  (map (lambda (z) (append (list (car z)) (list (cons #\T ch) (cons #\A (string-ref (car single-letter-word-list) 0))) (cdr z))) (map (lambda (x) (zip '(#\E #\I) x)) (permutations (remove ch (remove (string-ref (car single-letter-word-list) 0) list-most-frequent-five))))))

(define (give-i-single pref-order)
  (if(null? pref-order) '()
     (cons (i-helper (car pref-order)) (give-i-single (cdr pref-order)))))

(define (i-helper ch)
  (map (lambda (z) (append (list (car z)) (list (cons #\T ch)) (cdr z) (list (cons #\I (string-ref (car single-letter-word-list) 0))))) (map (lambda (x) (zip '(#\E #\A) x)) (permutations (remove ch (remove (string-ref (car single-letter-word-list) 0) list-most-frequent-five))))))

(define (a-first-ele pref-order)
  (if(null? pref-order) '()
     (cons (a-first-helper (car pref-order)) (a-first-ele (cdr pref-order)))))
(define (a-first-helper ch)
  (map (lambda (z) (append (list (car z)) (list (cons #\T ch) (cons #\A (string-ref (car single-letter-word-list) 0)) (cons #\I (string-ref (cadr single-letter-word-list) 0))))) (map (lambda (x) (zip '(#\E) x)) (permutations (remove ch (remove (string-ref (car single-letter-word-list) 0) (remove (string-ref (cadr single-letter-word-list) 0) list-most-frequent-five)))))))

(define (i-first-ele pref-order)
  (if(null? pref-order) '()
     (cons (i-first-helper (car pref-order)) (i-first-ele (cdr pref-order)))))

(define (i-first-helper ch)
  (map (lambda (z) (append (list (car z)) (list (cons #\T ch) (cons #\A (string-ref (cadr single-letter-word-list) 0)) (cons #\I (string-ref (car single-letter-word-list) 0))))) (map (lambda (x) (zip '(#\E) x)) (permutations (remove ch (remove (string-ref (car single-letter-word-list) 0) (remove (string-ref (cadr single-letter-word-list) 0) list-most-frequent-five)))))))

;; A suggested composition of strategies that might work well. Has not been
;; exhaustively tested by us. Be original ;)
(define composition (list etai))
                  ;; common-words-double
                  ;; bigrams
                  ;; common-initial-letters
                  ;; common-final-letters
                  ;; common-words-triple
                  ;; trigrams
                  ;; common-double-letters))
                  ;; common-words-quadruple
                  ;; quadgrams))

