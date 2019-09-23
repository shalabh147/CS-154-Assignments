#lang racket

;; You can require more modules of your choice.
(require racket/string
         racket/list
         (prefix-in utils: "utils.rkt"))

(provide secret-word-enumeration)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                           ;;
;; Secret Word Enumeration                                                                   ;;
;; =======================                                                                   ;;
;;                                                                                           ;;
;; This step exploits the fact that all keys begin with a secret word that is a              ;;
;; proper SIX-LETTER word from the English dictionary.                                       ;;
;;                                                                                           ;;
;; Given a partial key, we can query the dictionary for a list of 6 letter words             ;;
;; that can potentially begin the key.                                                       ;;
;; We can then filter out potential candidates whose keys do not agree with our partial key. ;;
;;                                                                                           ;;
;; It is possible that given a (wrong) partial key, we discover that none of these           ;;
;; potential candidates can agree with our partial key. This really implies a                ;;
;; mistake in the choice of substitution, since dictionary-closure is completely             ;;
;; deterministic (modulo any bugs in your implementation :)                                  ;;
;;                                                                                           ;;
;; Hence, this function must return either of:                                               ;;
;; a. `#f` if there are no consistent candidates for this key.                               ;;
;; b. the original key if there are multiple consistent candidates.                          ;;
;; c. the complete key if there's only one consistent candidate!                             ;;
;;                                                                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (secret-word-enumeration key-after-dictionary-closure)
  (define partial-key (reverse (list-tail (reverse key-after-dictionary-closure) 20)))                              ;;this line is to create first 6 letters of the key
  (if(all-alphabets partial-key) (cond[(check-duplicates (for/list ([subst-pair partial-key]) subst-pair)) #f]      ;;if no underscore in first 6 then check for duplicates in starting 6(eg. "secret","define")
                                        [else (utils:encryption-key (list->string partial-key))])
  (let ([a (swe-helper (string->list (string-upcase (list->string key-after-dictionary-closure))) utils:dictionary 0 '())])
    (cond[(= (cdr a) 0) #f]
         [(= (cdr a) 1) (utils:encryption-key (convert-to-string (caar a) '()))]
         [else key-after-dictionary-closure]))))
  ;(
  ;; Returns a key or false (#f)
  
(define (convert-to-string list-of-pairs l)
  (if(null? list-of-pairs)  (list->string (reverse l))
     (convert-to-string (cdr list-of-pairs) (cons (cdar list-of-pairs) l))))

(define (all-alphabets partial-key)
  (if(null? partial-key) #t
     (if(char-alphabetic? (car partial-key)) (all-alphabets (cdr partial-key)) #f)))

(define (swe-helper key-after-dict-closure dict c tr-list)
  
  (if(null? dict) (cons (key-non-repitant? tr-list (list-tail key-after-dict-closure 6) ) (length (key-non-repitant? tr-list (list-tail key-after-dict-closure 6) )))
  (if(= 6 (string-length (car dict))) (cond[(checker key-after-dict-closure (string->list (car dict))) (swe-helper key-after-dict-closure (cdr dict) (+ c 1) (cons (zip key-after-dict-closure (string->list (car dict))) tr-list))]
                                                              [else (swe-helper key-after-dict-closure (cdr dict) c tr-list)])
  (swe-helper key-after-dict-closure (cdr dict) c tr-list))))

(define (key-non-repitant? sw-suggestions-list last-20-letters)                            ;;this func is to check wheter the word formed by first 6 letters does not have any letter common with remaining 20
  (if(null? sw-suggestions-list) '()
     (cond[(locally-consistent (zip (append (list-formed-after-sw-helper (car sw-suggestions-list)) last-20-letters) upper-case-list)) (cons (car sw-suggestions-list) (key-non-repitant? (cdr sw-suggestions-list) last-20-letters))]
          [else (key-non-repitant? (cdr sw-suggestions-list) last-20-letters)])))

(define upper-case-list (build-list 26 (lambda (i) (integer->char (+ i 65)))))
(define (list-formed-after-sw-helper l)
  (if(null? l) '()
     (cons (cdr (car l)) (list-formed-after-sw-helper (cdr l)))))

(define (checker partial-key dict-word)
  (if(null? dict-word) #t
  (if(char-alphabetic? (car partial-key)) (cond[(equal? (car partial-key) (car dict-word)) (checker (cdr partial-key) (cdr dict-word))]
                                               [else #f])
  (checker (cdr partial-key) (cdr dict-word)))))

(define (zip l1 l2)
  (if(or (null? l1) (null? l2)) '()
     (cons (cons (car l1) (car l2)) (zip (cdr l1) (cdr l2)))))

(define (locally-consistent substitution)
  (not (or
        (check-duplicates (for/list ([subst-pair substitution])
                            (if(char-alphabetic? (car subst-pair)) (car subst-pair) (random 1000000))))     ;made some change so as to not give errors on finding two "#\_" to be duplicates
        (check-duplicates (for/list ([subst-pair substitution])
                            (cdr subst-pair))))))

