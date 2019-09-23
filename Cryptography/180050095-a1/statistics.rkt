#lang racket/base

;; You can require more modules of your choice.
(require racket/list
         racket/string
         (prefix-in utils: "utils.rkt"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                 ;;
;; Ciphertext Statistics                                                           ;;
;; =====================                                                           ;;
;;                                                                                 ;;
;; The module should provide a bunch of functions that do statistical analysis of  ;;
;; ciphertext. The output is almost always just an ordered list (counts are        ;;
;; omitted).                                                                       ;;
;;                                                                                 ;;
;; Fill in the body for the skeletons and do not change the arguments. You can     ;;
;; define as many functions as you require, there's no special credit for          ;;
;; implementing/using all of them.                                                 ;;
;;                                                                                 ;;
;; CAUTION:                                                                        ;;
;; 1. Be mindful that bi, tri and quadgrams do not cross word boundaries. Hence,   ;;
;; you must process each word separately.                                          ;;
;;                                                                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Analyses
(provide cipher-monograms
         cipher-bigrams
         cipher-unique-neighbourhood
         cipher-neighbourhood
         cipher-trigrams
         cipher-quadgrams
         cipher-common-words-single
         cipher-common-words-double
         cipher-common-words-triple
         cipher-common-words-quadruple
         cipher-common-initial-letters
         cipher-common-final-letters
         cipher-common-double-letters
         ;; any other functions of your design come below:

         ;; my-fundoo-analysis
         )

;; Takes ciphertext and produces a list of cipher chars sorted in decreasing
;; order of frequency.
(define (cipher-monograms ciphertext)
 (define l (sort (cipher-helper (string->list ciphertext) '()) comparator)) 
  (ret-first-ele l)
  )

 (define (cipher-helper l l1)              ;a tail recursive func to either add a new alphabet or increase already present frequency
   (if(null? l) l1
   (if(char-alphabetic? (car l)) (cond[(if-already-present? (car l) l1) (cipher-helper (cdr l) (operate (car l) l1))]
                                      [else (cipher-helper (cdr l) (cons (cons (car l) 1) l1))])
      (cipher-helper (cdr l) l1))))
  (define (if-already-present? q l)
    (if(null? l) #f
       (if(equal? (caar l) q) #t
          (if-already-present? q (cdr l)))))
  (define (operate q l)
    (operatec q l '()))
  (define (operatec q l l1)
    (if(equal? (caar l) q) (append (reverse l1) (list (cons (caar l) (+ 1 (cdar l)))) (cdr l))
       (operatec q (cdr l) (cons (car l) l1))))

(define (comparator x y)
  (> (cdr x) (cdr y)))
(define (ret-first-ele l1)
  (if(null? l1) '()
     (cons (caar l1) (ret-first-ele (cdr l1)))))

;; Takes the cipher-word-list and produces a list of 2-letter bigram (strings)
;; sorted in decreasing order of frequency. Each element must be a string!
(define (cipher-bigrams cipher-word-list)
  (define l (sort (cipher-bi-helper cipher-word-list '()) comparator))
  (ret-first-ele l)
  )
(define (cipher-bi-helper l l1)
  (if(null? l) l1
     (cipher-bi-helper (cdr l) (f (car l) l1 (string-length (car l)) ))))
(define (f s l1 length2)
  (if(= (string-length s) 1) l1
     (cond[(if-already-present? (substring s 0 2) l1) (f (substring s 1 length2) (operate (substring s 0 2) l1) (- length2 1))]
          [else (f (substring s 1  length2) (cons (cons (substring s 0 2) 1) l1) (- length2 1))])))

;; Takes the bigram frequency order (output of `cipher-bigrams`) and computes
;; the neighbourhood of each letter with every other letter. Only unique
;; neighbours are to be counted.
;; Consider the character #\o.
;;
;; Takes an argument `mode`:
;; 1. (cipher-unique-neighbourhood cipher-bigrams 'predecessor)
;;    - Count only those unique occurrences where the (given) char preceeds
;;      other char.
;;    - Patterns of the form: "o?"
;; 2. (cipher-unique-neighbourhood cipher-bigrams 'successor)
;;    - Count only those unique occurrences where the (given) char succeeds
;;      other char.
;;    - Patterns of the form: "?o"
;; 3. (cipher-unique-neighbourhood cipher-bigrams 'both)
;;    - Count all unique occurrences where the (given) char neighbours the other
;;      char.
;;    - Patterns of the form "?o" and "o?". Note that this is not sum of (1) and
;;    (2) always.
;;
;; The output is a list of pairs of cipher char and the count of it's
;; neighbours. The list must be in decreasing order of the neighbourhood count.
(define (cipher-unique-neighbourhood cipher-bigrams-list mode)
 (define l3 (sort (cond[(equal? mode 'predecessor) (predec-checking cipher-bigrams-list l5)]
       [(equal? mode 'successor) (success-checking cipher-bigrams-list l5)]
       [(equal? mode 'both) (both-checking cipher-bigrams-list l5)]) comparator))
  l3)

 (define l5 (string->list (build-string 26 (lambda (x) (integer->char (+ x 97))))))
(define (predec-checking l l2)
  (if(null? l2) '()
  (cons (pre-helper (car l2) l 0) (predec-checking l (cdr l2)))))

(define (pre-helper ch l freq)
  (if(null? l) (cons ch freq)
     (cond[(equal? (string-ref (car l) 0) ch) (pre-helper ch (cdr l) (+ freq 1))]
          [else (pre-helper ch (cdr l) freq)])))

(define (success-checking l l2)
  (if(null? l2) '()
  (cons (suc-helper (car l2) l 0) (success-checking l (cdr l2)))))
(define (suc-helper ch l freq)
  (if(null? l) (cons ch freq)
     (cond[(equal? (string-ref (car l) 1) ch) (suc-helper ch (cdr l) (+ freq 1))]
          [else (suc-helper ch (cdr l) freq)])))

(define (both-checking l l2)
  (if(null? l2) '()
  (cons (both-helper (car l2) l 0) (both-checking l (cdr l2)))))
(define (both-helper ch l freq)
   (if(null? l) (cons ch freq)
     (cond[(or (equal? (string-ref (car l) 1) ch) (equal? (string-ref (car l) 0) ch)) (both-helper ch (cdr l) (+ freq 1))]
          [else (both-helper ch (cdr l) freq)])))

  

  ;; You must match against or test (using cond) for the `mode` argument. Possibilities are:
  ;; 'predecessor, 'successor, 'both
  ;; Figure out experimentally which of these is a good indicator for E vs T.
  

;; Takes the bigram frequency order (output of `cipher-bigrams`) and computes
;; the neighbourhood of each letter with every other letter, but counts each
;; occurrence distinctly. This comment contains 6 bigrams with "w", all with "i" or "h".
;; So "w" has:
;; when mode is 'both,        6 neighbours
;; when mode is 'predecessor, 6 neighbours
;; when mode is 'successor,   0 neighbours
(define (cipher-neighbourhood cipher-word-list mode)
  (define l2 (sort (cipher-bi-helper cipher-word-list '()) comparator))
  (define final-list (sort (cond[(equal? mode 'predecessor) (predec-nu-checking l2 l5)]
                                [(equal? mode 'successor) (success-nu-checking l2 l5)]
                                [else (both-nu-checking l2 l5)]) comparator))
final-list)

(define (predec-nu-checking bigrams-list alpha)
  (if(null? alpha) '()
     (cons (predec-nu-helper (car alpha) bigrams-list 0) (predec-nu-checking bigrams-list (cdr alpha)))))

(define (predec-nu-helper ch bigrams-list freq)
  (if(null? bigrams-list) (cons ch freq)
     (cond[(equal? ch (string-ref (caar bigrams-list) 0)) (predec-nu-helper ch (cdr bigrams-list) (+ freq (cdar bigrams-list)))]
          [else (predec-nu-helper ch (cdr bigrams-list) freq)])))

(define (success-nu-checking bigrams-list alpha)
  (if(null? alpha) '()
     (cons (success-nu-helper (car alpha) bigrams-list 0) (success-nu-checking bigrams-list (cdr alpha)))))

(define (success-nu-helper ch bigrams-list freq)
  (if(null? bigrams-list) (cons ch freq)
     (cond[(equal? ch (string-ref (caar bigrams-list) 1)) (success-nu-helper ch (cdr bigrams-list) (+ freq (cdar bigrams-list)))]
          [else (success-nu-helper ch (cdr bigrams-list) freq)])))


(define (both-nu-checking bigrams-list alpha)
  (if(null? alpha) '()
     (cons (both-nu-helper (car alpha) bigrams-list 0) (both-nu-checking bigrams-list (cdr alpha)))))

(define (both-nu-helper ch bigrams-list freq)
  (if(null? bigrams-list) (cons ch freq)
     (cond[(or (equal? ch (string-ref (caar bigrams-list) 1)) (equal? ch (string-ref (caar bigrams-list) 0))) (both-nu-helper ch (cdr bigrams-list) (+ freq (cdar bigrams-list)))]
          [else (both-nu-helper ch (cdr bigrams-list) freq)])))


  ;; You must match against or test (using cond) for the `mode` argument. Possibilities are:
  ;; 'predecessor, 'successor, 'both
  ;; Figure out experimentally which of these is a good indicator for E vs T.
  

;; Takes the cipher-word-list and produces a list of 3-letter bigram (strings)
;; sorted in decreasing order of frequency. Each element must be a string!
(define (cipher-trigrams cipher-word-list)
  '())

;; Takes the cipher-word-list and produces a list of 4-letter bigram (strings)
;; sorted in decreasing order of frequency. Each element must be a string!
(define (cipher-quadgrams cipher-word-list)
  '())

;; Takes the cipher word list and produces a list of single letter words, sorted
;; in decreasing order of frequency. Each element must be a string!
(define (cipher-common-words-single cipher-word-list)
 (define l (sort (c-helper cipher-word-list '() ) comparator))
  (ret-first-ele l))
(define (c-helper l l2)
  (if(null? l) l2
     (cond[(= (string-length (car l)) 1)  (cond[(if-already-present? (car l) l2) (c-helper (cdr l) (operate (car l) l2))]
                                      [else (c-helper (cdr l) (cons (cons (car l) 1) l2))])]
          [else (c-helper (cdr l) l2)])))
          

;; Takes the cipher word list and produces a list of double letter words, sorted
;; in decreasing order of frequency.
(define (cipher-common-words-double cipher-word-list)
  '())

;; Takes the cipher word list and produces a list of triple letter words, sorted
;; in decreasing order of frequency.
(define (cipher-common-words-triple cipher-word-list)
  '())

;; Takes the cipher word list and produces a list of four letter words, sorted
;; in decreasing order of frequency.
(define (cipher-common-words-quadruple cipher-word-list)
  '())

;; Takes the cipher word list and produces a list of chars that appear at the
;; start of words, sorted in decreasing order of frequency. Each element must be
;; a char!
(define (cipher-common-initial-letters cipher-word-list)
  '())

;; Takes the cipher word list and produces a list of chars that appear at the
;; end of words, sorted in decreasing order of frequency. Each element must be
;; a char!
(define (cipher-common-final-letters cipher-word-list)
  '())

;; Takes the cipher word list and produces a list of chars that appear as
;; consecutive double letters in some word, sorted in decreasing order of
;; frequency. Each element must thus be a char!
(define (cipher-common-double-letters cipher-word-list)
  '())
