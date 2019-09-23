
#lang racket
;(require "testdata.rkt")
(require "decision_functions.rkt")

(require 2htdp/batch-io)

;input dataset
(provide toytrain)
(define toytrain "../data/toy_train.csv")

(provide titanictrain)
(define titanictrain "../data/titanic_train.csv") 

(provide mushroomtrain)
(define mushroomtrain "../data/mushrooms_train.csv") 


;output tree (dot file)
(provide toyout)
(define toyout "../output/toy-decision-tree4.dot")

;reading input datasets
;read the csv file myfile as a list of strings
;with each line of the original file as an element of the list
;further split each line at commas
;so then we have a list of list of strings
(provide toy-raw)
(define toy-raw (read-csv-file toytrain))

(provide titanic-raw)
(define titanic-raw (map (lambda (x) (cdr (cdr x))) (read-csv-file titanictrain)))

(provide mushroom-raw)
(define mushroom-raw (read-csv-file mushroomtrain))

;function to convert data to internal numerical format
;(features . result)
(provide format)
(define (format data)
  (cons (map (lambda (x) (string->number x)) (cdr data)) (string->number (car data))))

;list of (features . result)

(define (toy-helper f l)
  (if(null? l) '()
     (cons (f (car l)) (toy-helper f (cdr l)))))

(provide toy)
(define toy (toy-helper format (cdr toy-raw)))


(define (titanic-helper f l)
  (if(null? l) '()
     (cons (f (car l)) (titanic-helper f (cdr l)))))

(provide titanic)
(define titanic (titanic-helper format (cdr titanic-raw)))

(define (mushroom-helper f l)
  (if(null? l) '()
     (cons (f (car l)) (mushroom-helper f (cdr l)))))

(provide mushroom)
(define mushroom (mushroom-helper format (cdr mushroom-raw)))


;============================================================================================================
;============================================================================================================
;============================================================================================================

;get fraction of result fields that are 1
;used to find probability value at leaf
(provide get-leaf-prob)
(define (get-leaf-prob data)
  (leaf-helper 0 0 data))

(define (leaf-helper c k data)
  (cond[(null? data) (/ c k)]
       [else (cond[(equal? (cdar data) 1) (leaf-helper (+ c 1) (+ k 1) (cdr data))]
                  [else (leaf-helper c (+ k 1) (cdr data))])]))

;get entropy of dataset
(provide get-entropy)
(define (get-entropy data)
  (define x (entropy-calculator (get-leaf-prob data))) 
  (* -1 x))

(define (entropy-calculator y)
  (+ (* y (log-calc y)) (* (- 1 y) (log-calc (- 1 y)))))

(define (log-calc t)
  (if(= t 0) 0
  (log t 2)))

;find the difference in entropy achieved
;by applying a decision function f to the data
(provide entropy-diff)
(define (entropy-diff f data)
  (define l (remove-duplicates (map car (sort (map (lambda (x) (cons (f (car x)) (cdr x))) data) comparator))))
  (define binary-pair (0-1founder data))
  (define m (map (lambda (x) (number-matcher x data f 0 0)) l))
  (define new-entropy (expected-entropy binary-pair m 0))
  (define old-entropy (get-entropy data))
  (- old-entropy new-entropy)
  
  )
(define (comparator p1 p2)
  (< (car p1) (car p2)))

(define (0-1founder data)
  (define x (map cdr data))
  (founder-helper x 0 0))

(define (founder-helper binary-list c k)
  (if(null? binary-list) (cons c k)
     (cond[(= (car binary-list) 0) (founder-helper (cdr binary-list) (+ c 1) k)]
          [else (founder-helper (cdr binary-list) c (+ k 1))])))

(define (number-matcher x data f c k)
  (if(null? data) (cons c k)
     (cond[(= (f (caar data)) x) (cond[(= 0 (cdr (car data))) (number-matcher x (cdr data) f (+ c 1) k)]
                                            [(= 1 (cdr (car data))) (number-matcher x (cdr data) f c (+ k 1))])]
          [else (number-matcher x (cdr data) f c k)])))

(define (full-producer x data f l)
  (filter (lambda (y) (= (f (car y)) x)) data))
  
(define (expected-entropy parent-pair list-kids abs-entropy)
  (if(null? list-kids) (- abs-entropy)
     (expected-entropy parent-pair (cdr list-kids) (+ abs-entropy (* (/ (+ (caar list-kids) (cdar list-kids)) (+ (car parent-pair) (cdr parent-pair))) (entropy-calculator (/ (cdar list-kids) (+ (cdar list-kids) (caar list-kids)))))))))
  


;choose the decision function that most reduces entropy of the data
(provide choose-f)
(define (choose-f candidates data)
  (if(null? candidates) '() ; returns a decision function
  (list-ref candidates (choose-c candidates data 0 0 0))
  ))

(define (choose-c list-of-func data max-ent-diff max-index current-index)
  (if(null? list-of-func) max-index
     (let ([x (entropy-diff (cdr (car list-of-func)) data)])
     (cond[(>= x max-ent-diff) (choose-c (cdr list-of-func) data x current-index (+ 1 current-index))]
          [else (choose-c (cdr list-of-func) data max-ent-diff max-index (+ 1 current-index))]))))
(define (list-producer f data)
(remove-duplicates (map car (sort (map (lambda (x) (cons (f (car x)) (cdr x))) data) comparator))))

(provide DTree)
(struct DTree (desc func kids))

;build a decision tree (depth limited) from the candidate decision functions and data
(provide build-tree)
(define (build-tree candidates data depth)
  (define a (choose-f candidates data))
  (define prob (get-leaf-prob data))
  (cond[(= depth 0) (DTree (number->string prob) '() '())]
       [(null? candidates) (DTree (number->string prob) '() '())]
       [else (DTree (car a) (cons a (list-producer (cdr a) data)) (map (lambda (x) (build-tree (remove a candidates) (full-producer x data (cdr a) '()) (- depth 1)))
                                          (list-producer (cdr a) data)))]))                                    

;given a test data (features only), make a decision according to a decision tree
;returns probability of the test data being classified as 1
(provide make-decision)
(define (make-decision tree test)
  (match tree [(DTree s f k) (cond[(null? k) (string->number s)]
                                  [else (let([m (func-to-extract-kid (car f) k (cdr f) test)])
                                          (cond[(equal? m 0) 0]
                                               [else (make-decision (func-to-extract-kid (car f) k (cdr f) test) test)]))]
                                                   )]))

(define (func-to-extract-kid f k data test)
 ; (define m (list-producer (cdr f) data))
  (define x (index-finder data ((cdr f) test) 0))
      (cond[(equal? x #f) 0]
           [else (list-ref k x)]))
  


(define (index-finder m element c)
  (if(null? m) #f
  (if(= element (car m)) c
     (index-finder (cdr m) element (+ c 1)))))
  

;============================================================================================================
;============================================================================================================
;============================================================================================================

;annotate list with indices
(define (pair-idx lst n)
  (if (empty? lst) `() (cons (cons (car lst) n) (pair-idx (cdr lst) (+ n 1))))
  )

;generate tree edges (parent to child) and recurse to generate sub trees
(define (dot-child children prefix tabs)
  (apply string-append
         (map (lambda (t)
                (string-append tabs
                               "r" prefix
                               "--"
                               "r" prefix "t" (~a (cdr t))
                               "[label=\"" (~a (cdr t)) "\"];" "\n"
                               (dot-helper (car t)
                                           (string-append prefix "t" (~a (cdr t)))
                                           (string-append tabs "\t")
                                           )
                               )
                ) children
                  )
         )
  )


;generate tree nodes and call function to generate edges
(define (dot-helper tree prefix tabs)
  (let* ([node (match tree [(DTree d f c) (cons d c)])]
         [d (car node)]
         [c (cdr node)])
    (string-append tabs
                   "r"
                   prefix
                   "[label=\"" d "\"];" "\n\n"
                   (dot-child (pair-idx c 0) prefix tabs)
                   )
    )
  )




;output tree (dot file)
(provide display-tree)
(define (display-tree tree dtfile)
  (write-file dtfile (string-append "graph \"decision-tree\" {" "\n" (dot-helper tree "" "\t") "}"))
  )

(define dotfile
  (display-tree (build-tree (list y1 y2 y3) toy 3) toyout)
  )

;(define test1
 ; (let* ([train toy]
  ;       [test toy_test]
   ;      [candidates (list y1 y2 y3)]
    ;     [dtree (build-tree candidates train 3)])
    ;(map (lambda (x) (make-decision dtree x)) test)
    ;)
  ;)
;
;(define test2
;  (let* ([train titanic]
;         [test titanic_test]
;         [candidates (list pclass sex age>25 sibsp parch fare>50 emb)]
;         [dtree (build-tree candidates train 5)])
;    (map (lambda (x) (make-decision dtree x)) test)
;    )
;  )
;
;(define test3
;  (let* ([train mushroom]
;         [test mushroom_test]
;         [candidates (list cshape csurf bruise odor gatch gspace gsize sshape nring pop hab)]
;         [dtree (build-tree candidates train 8)])
;    (map (lambda (x) (make-decision dtree x)) test)
;    )
;  )
;============================================================================================================
;============================================================================================================
;============================================================================================================