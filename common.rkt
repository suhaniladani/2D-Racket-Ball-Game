;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname common) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; general-recursion for merge, merge-sort

(require rackunit)
(require "extras.rkt")
(check-location "08" "common.rkt")
(provide merge-sort
         remove-dups
         appending)


;; DATA DEFINITION:
;; A SortedList is a list of Reals, sorted by <.  Duplicates are
;; allowed. 

;; merge : SortedList SortedList -> SortedList
;; merges its two arguments
;; STRATEGY: recur on either (rest lst1) or (rest lst2)
;; HALTING MEASURE: length of lst1 + length of lst2

(define (merge lst1 lst2)
  (cond
    [(empty? lst1) lst2]
    [(empty? lst2) lst1]
    [(string<? (first lst1) (first lst2))
     (cons (first lst1) (merge (rest lst1) lst2))]
    [else
     (cons (first lst2) (merge lst1 (rest lst2)))]))


;; merge-sort : RealList -> SortedList
;; GIVEN: a list of numbers
;; RETURNS: a sorted version of the same list
;; EXAMPLES:
;; empty => empty
;; (list 4 2 6 7 6 8) => (list 2 4 6 6 7 8)
;; STRATEGY: recur on even elements and odd elements, then merge
;; results.
;; HALTING MEASURE: (length lst)

(define (merge-sort lst)
  (cond
    [(empty? lst) lst]
    [(empty? (rest lst)) lst]
    [else
      (local
       ((define evens (odd-elements lst))
        (define odds  (even-elements lst)))
       (merge 
        (merge-sort evens)
        (merge-sort odds)))]))

;; odd-elements : RealList -> RealList
;; Strategy: Recur on (rest (rest lst))
(define (odd-elements lst)
  (cond
    [(empty? lst) empty]
    [(empty? (rest lst)) (list (first lst))]
    [else (cons
            (first lst)
            (odd-elements (rest (rest lst))))]))

;; even-elements : RealList -> RealList
;; Strategy: Recur on (rest (rest lst))
(define (even-elements lst)
  (cond
    [(empty? lst) empty]
    [(empty? (rest lst)) empty]
    [else (cons
            (first (rest lst))
            (even-elements (rest (rest lst))))]))

;; INSERT EVEN-ELEMENTS AND ODD-ELEMENTS HERE


;; remove-dups : StringList StringList -> StringList
;; GIVEN : Two StringList
;; RETURN : StringList which is List of Unique Variables defined by the Block
;; lst   : StringList  -- Variables defined by the Block
;; unique-lst : StringList -- Unique Variables defined by the Block
;; EXAMPLES : (list "x" "z" "x") => (list "x" "z")
;; STRATEGY : use recursion on list  

(define (remove-dups lst unique-lst)
  (cond
    [(empty? lst) unique-lst]
    [(not (member? (first lst) unique-lst))
     (remove-dups (rest lst) (cons (first lst) unique-lst))]
    [else (remove-dups (rest lst) unique-lst)]))

;; appending : OutcomeList -> OutcomeList
;; GIVEN : A list of list of outcomes
;; RETURNS : A single list of outcomes with all the outcomes appended
;; EXAMPLES :
;; (list (list (list "C" "D") (list "D" "C"))
;;         (list (list "A" "B") (list "B" "A"))) =>
;;       (list (list "C" "D") (list "D" "C") (list "A" "B") (list "B" "A"))
;; STRATEGY : use recursive call on outcome-list
;; HALTING MEASURE: (length outcome-list)

(define (appending outcome-list)
  (cond
    [(empty? outcome-list) empty]
    [else (append (first outcome-list) (appending (rest outcome-list)))]))


(begin-for-test
  (check-equal? (merge-sort (list "d" "b" "a" "c")) (list "a" "b" "c" "d"))

  (check-equal? (appending (list (list (list "a" "b") (list "g" "h"))
                                 (list (list "c" "d") (list "e" "f"))))
                (list (list "a" "b") (list "g" "h") (list "c" "d")
                      (list "e" "f")))
  (check-equal? (remove-dups (list "a" "b" "c" "a") empty)
                (list "c" "b" "a"))
  (check-equal? (merge-sort '()) '())
  (check-equal? (merge-sort (list "b" "a" "c")) (list "a" "b" "c")))
