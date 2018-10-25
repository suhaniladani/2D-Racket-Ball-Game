;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname q2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")
(require "q1.rkt")
(require "common.rkt")
(check-location "08" "q2.rkt")

(provide tie
         defeated
         defeated?
         outranks
         outranked-by
         power-ranking)


;; PowerRank is represented as struct
;; (make-power-rank competitor outranked-by outranks non-losing)
;; with the fields
;; - competitor : Competitor
;; - outranked-by : Integer that represents number of competitors that outranks
;;                  the given competitor
;; - outranks   : Integer that represents number of competitors that the given
;;                competitor outranks
;; - non-losing : PosReal number that represents the non-losing percentage of 
;;                the given Competitor

;; IMPLEMENTATION
(define-struct power-rank (competitor outranked-by outranks non-losing))

;; CONSTRUCTOR TEMPLATE
;; (make-power-rank Competitor Integer Integer PosReal)

;; OBSERVER TEMPLATE
;; power-rank-fn : PowerRank -> ?
(define (power-rank-fn pr)
  (...
   (power-rank-competitor pr)
   (power-rank-outranked-by pr)
   (power-rank-outranks pr)
   (power-rank-non-losing pr)))


;; PowerRankList could be one of the following:
;; - empty
;; - List of PowerRank

;; CONSTRUCTOR TEMPLATE:
;; (cons PowerRank PowerRankList)

;;; OBSERVER TEMPLATE:
;;; power-rank-list-fn : PowerRankList -> ??
#;
(define (power-rank-list-fn prlst)
  (cond
    [(empty? prlst) empty]
    [else (... (power-rank-fn (first prlst))
               (power-rank-list-fn (rest prlst)))]))


;;; power-ranking : OutcomeList -> CompetitorList
;;; GIVEN: a list of outcomes
;;; RETURNS: a list of all competitors mentioned by one or more
;;;     of the outcomes, without repetitions, with competitor A
;;;     coming before competitor B in the list if and only if
;;;     the power-ranking of A is higher than the power ranking
;;;     of B.
;;; EXAMPLE:
;;;     (power-ranking
;;;      (list (defeated "A" "D")
;;;            (defeated "A" "E")
;;;            (defeated "C" "B")
;;;            (defeated "C" "F")
;;;            (tie "D" "B")
;;;            (defeated "F" "E")))
;;;  => (list "C"   ; outranked by 0, outranks 4
;;;           "A"   ; outranked by 0, outranks 3
;;;           "F"   ; outranked by 1
;;;           "E"   ; outranked by 3
;;;           "B"   ; outranked by 4, outranks 12, 50%
;;;           "D")  ; outranked by 4, outranks 12, 50%
;;; STRATGEGY : combine simpler functions

(define (power-ranking outcome-list)
  (pr-competitors
   (sorting
    (find-power-ranking outcome-list
                        (remove-dups
                         (appending (appending outcome-list)) empty)))))


;; pr : Competitor OutcomeList -> PowerRank
;; GIVEN : A Competitor and a list of Outcomes
;; RETURNS : PowerRank of the given Competitor
;; EXAMPLES :
;;        (pr "A" (list (defeated "A" "D")
;;            (defeated "A" "E")
;;            (defeated "C" "B")
;;            (defeated "C" "F")
;;            (tie "D" "B")
;;            (defeated "F" "E"))) =>
;;       (make-power-rank "A" 2 2 100)
;; STRATEGY : use constructor template for PowerRank

(define (pr competitor outcome-list)
  (make-power-rank
   competitor
   (length (outranked-by competitor outcome-list))
   (length (outranks competitor outcome-list))
   (non-losing competitor (appending outcome-list) outcome-list)))


;; find-power-ranking : OutcomeList Competitors -> PowerRankList
;; GIVEN : List of Outcomes and List of Competitors
;; RETURNS : List of PowerRank of each of the Competitor in the list
;; EXAMPLES :
;;       (find-power-ranking (list (defeated "A" "D")
;;                                 (defeated "A" "E")
;;                                 (defeated "C" "B")
;;                                 (defeated "C" "F")
;;                                 (tie "D" "B")
;;                                 (defeated "F" "E"))
;;                          (list "A" "B" "C" "D" "E" "F")) =>
;;                  
;;               (make-power-rank "C" 0 4 100)
;;               (make-power-rank "A" 0 3 100)
;;               (make-power-rank "F" 1 1 50)
;;               (make-power-rank "E" 3 0 0)
;;               (make-power-rank "B" 4 2 50)
;;               (make-power-rank "D" 4 2 50))
;; STRATGEY : structural recursive call on outcome-list

(define (find-power-ranking outcome-list competitors)
  (cond
    [(empty? competitors) empty]
    [else (append (cons (pr (first competitors) outcome-list) empty)
                  (find-power-ranking outcome-list (rest competitors)))]))

;; NonLosingPercentage : PosReal number that represents the Non-losing
;;                       Percentage of the given Competitor
;; non-losing : Competitor OutcomeList OutcomeList -> NonLosingPercentage
;; GIVEN : A Competitor and an appended OutcomeList and an OutcomeList as it is
;; WHERE : the non-losing percentage of a competitor A is the number of outcomes
;;         in which given Competitor defeats or ties another competitor
;;         divided by the number of outcomes that mention the given Competitor
;; RETURNS : the NonLosingPercentage of the given Competitor
;; EXAMPLES :
;;          (non-losing "A"
;;                      (list (list "A" "D") (list "A" "E") (list "C" "B")
;;                      (list "C" "F") (list "D" "B") (list "B" "D")
;;                      (list "F" "E"))
;;                      (list
;;                      (list (list "A" "D"))
;;                      (list (list "A" "E"))
;;                      (list (list "C" "B"))
;;                      (list (list "C" "F"))
;;                      (list (list "D" "B") (list "B" "D"))
;;                      (list (list "F" "E")))  => 100
;; STRATEGY : combine simpler functions 

(define (non-losing  competitor appended-outcome-list outcome-list)
  (* (/ (calculate-defeated competitor appended-outcome-list 0)
        (calculate-denominator competitor outcome-list 0)) 100))

;; calculate-defeated : Competitor OutcomeList Integer -> Integer
;; GIVEN : A Competitor, an OutcomeList and an Integer that represents the
;;         number of times that the given competitor defeats or ties other
;;         competitors
;; RETURNS : Integer that represents the final calculated number of times
;;           that the given competitor defeats or ties other competitors
;; EXAMPLES :
;;   (calculate-defeated "A"
;;                      (list (list "A" "D") (list "A" "E") (list "C" "B")
;;                      (list "C" "F") (list "D" "B") (list "B" "D")
;;                      (list "F" "E")) 0) => 2
;;    
;; STRATEGY : make recursive call on outcome-list


(define (calculate-defeated competitor appended-outcome-list x)
  (cond
    [(empty? appended-outcome-list) x]
    [(equal? (first (first appended-outcome-list)) competitor)
     (calculate-defeated competitor (rest appended-outcome-list) (add1 x))]
    [else (calculate-defeated competitor (rest appended-outcome-list) x)]))

;; calculate-denominator : Competitor OutcomeList Integer -> Integer
;; GIVEN : A Competitor, an OutcomeList and an Integer that represents the
;;         number of Outcomes that mentions the given competitor 
;; RETURNS : Integer that represents the final calculated number of Outcomes
;;           that mentions the given competitor 
;; EXAMPLES :
;;         (calculate-defeated "A"
;;                         (list (list "A" "D") (list "A" "E") (list "C" "B")
;;                         (list "C" "F") (list "D" "B") (list "B" "D")
;;                         (list "F" "E")) 0) => 2
;; STRATEGY : use cases on outcome-list
;;            to make a recursive call on outcome-list

(define (calculate-denominator competitor outcome-list x)
  (cond
    [(empty? outcome-list) x]
    [(member? competitor (appending (first outcome-list)))
     (calculate-denominator competitor (rest outcome-list) (add1 x))]
    [else (calculate-denominator competitor (rest outcome-list) x)]))

;; sorting : OutcomeList -> OutcomeList
;; GIVEN : unsorted List of Outcomes
;; RETURNS : A sorted list of Outcomes according to the number of outranks,
;;          outranked-by, non-losing percetage and Alphabetical order
;; EXAMPLES :
;;      (sorting (list
;;                (make-power-rank "A" 0 3 100)
;;                (make-power-rank "B" 4 2 50)
;;                (make-power-rank "C" 0 4 100)
;;                (make-power-rank "F" 1 1 50)
;;                (make-power-rank "E" 3 0 0)
;;                (make-power-rank "D" 4 2 50))) =>
;;              (list
;;                (make-power-rank "C" 0 4 100)
;;                (make-power-rank "A" 0 3 100)
;;                (make-power-rank "F" 1 1 50)
;;                (make-power-rank "E" 3 0 0)
;;                (make-power-rank "B" 4 2 50)
;;                (make-power-rank "D" 4 2 50))
;; STRATEGY : use HOF on PowerRankList

(define (sorting lst)
  (sort lst
        (
         ;; CONTRACT : PowerRankList -> PowerRankList
         ;; GIVEN : An unsorted PowerRankList
         ;; RETURNS : Boolean true if condition is satisfied
         lambda (pr1 pr2)
          (or
           (< (power-rank-outranked-by pr1) (power-rank-outranked-by pr2))
           (and
            (equal? (power-rank-outranked-by pr1) (power-rank-outranked-by pr2))
            (> (power-rank-outranks pr1) (power-rank-outranks pr2)))
           (sort-by-non-losing pr1 pr2)
           (sort-by-string pr1 pr2)))))

;; sort-by-non-losing : PowerRank PowerRank -> Boolean
;; GIVEN : two PowerRanks from the given list of PowerRankList
;; RETURNS : Boolean true if the condition is satisfied
;; EXAMPLES :
;;        (sort-by-string
;;                (make-power-rank "D" 4 2 50)
;;                (make-power-rank "B" 4 2 100)) => true
;; STRATEGY : use observer template of PowerRank on pr1 and pr2 

(define (sort-by-non-losing pr1 pr2)
  (and
   (equal? (power-rank-outranked-by pr1) (power-rank-outranked-by pr2))
   (equal? (power-rank-outranks pr1) (power-rank-outranks pr2))
   (> (power-rank-non-losing pr1) (power-rank-non-losing pr2))))

;; sort-by-string : PowerRank PowerRank -> Boolean
;; GIVEN : two PowerRanks from the given list of PowerRankList
;; RETURNS : Boolean true if the condition is satidfied
;; EXAMPLES :
;;        (sort-by-string
;;                (make-power-rank "D" 4 2 50)
;;                (make-power-rank "B" 4 2 50)) => true
;; STRATEGY : use observer template of PowerRank on pr1 and pr2

(define (sort-by-string pr1 pr2)
  (and
   (equal? (power-rank-outranked-by pr1) (power-rank-outranked-by pr2))
   (equal? (power-rank-outranks pr1) (power-rank-outranks pr2))
   (equal? (power-rank-non-losing pr1) (power-rank-non-losing pr2))
   (string<?
    (power-rank-competitor pr1) (power-rank-competitor pr2))))

;; CompetitorsList : List of Competitors
;; pr-competitors : PowerRankList -> CompetitorsList
;; GIVEN : A PowerRankList
;; RETURNS : A List of Competitors in the Power Ranking order
;; EXAMPLES :
;;         (pr-competitors
;;              (list
;;                (make-power-rank "C" 0 4 100)
;;                (make-power-rank "A" 0 3 100)
;;                (make-power-rank "F" 1 1 50)
;;                (make-power-rank "E" 3 0 0)
;;                (make-power-rank "B" 4 2 50)
;;                (make-power-rank "D" 4 2 50))=>
;;              (list "C" "A" "F" "E" "B" "D")
;; STRATEGY : use recursive call on pr-list

(define (pr-competitors pr-list)
  (cond
    [(empty? pr-list) empty]
    [else
     (append (cons (power-rank-competitor (first pr-list)) empty)
             (pr-competitors (rest pr-list)))]))

;; TESTS
(begin-for-test
  (check-equal? (power-ranking (list (defeated "A" "D")
                     (defeated "A" "E")
                     (defeated "C" "B")
                     (defeated "C" "F")
                     (tie "D" "B")
                     (defeated "F" "E")))
                (list "C" "A" "F" "E" "B" "D")
                "Arranged in descending order of Power ranking"))

