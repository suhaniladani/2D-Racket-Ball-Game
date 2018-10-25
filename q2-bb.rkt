#lang racket
(require racket/sandbox)
(require rackunit)
(require rackunit/text-ui)
(require "q2.rkt")



;; Test case utilities

(define TIME-LIMIT-S 3600)
(define MEMORY-LIMIT-MB 512)

(define-syntax limited-test-case
  (syntax-rules ()
    ((_ name body ...)
     (test-case name
                (with-limits TIME-LIMIT-S
                  MEMORY-LIMIT-MB
                  body ...)))))

;; Abbreviation
(define ++ string-append)



;;; canonical : StringList -> StringList
;;; GIVEN: a list of strings
;;; RETURNS: a list of those strings in some canonical order

(define (canonical ss)
  (sort ss string<?))


;; test case
(define tests
  (test-suite
   "q2"
   
   (limited-test-case
    "Test #1"
    ;power-ranking
    (check-equal?
     (power-ranking
      (list
       (defeated "B" "C")
       (defeated "C" "B")
       (tie "A" "B")
       (tie "A" "C")
       (defeated "C" "A")))
     (list "C" "A" "B")
     "C has higher non losing percentage than A"))
   
   
   (limited-test-case
    "Test #2"
    ;power-ranking
    (check-equal?
     (power-ranking
      (list
       (defeated "A" "B")
       (defeated "B" "C")
       (defeated "C" "D")
       (tie "D" "E")
       (defeated "E" "H")
       (tie "F" "I")
       (tie "G" "K")
       (defeated "H" "L")
       (defeated "I" "M")
       (tie "J" "N")
       (tie "P" "B")
       (tie "C" "E")
       (defeated "J" "P")
       (tie "Q" "P")
       (defeated "R" "K")
       (tie "S" "L")
       (defeated "T" "A")
       (defeated "U" "B")
       (defeated "V" "E")
       (defeated "W" "P")
       (tie "X" "B")
       (defeated "Y" "E")
       (defeated "Z" "P")))
     (list "T" "U" "W" "Z" "V" "Y" "R" "A" "J" "N" "F"
           "I" "M" "G" "K" "Q" "X" "B" "P" "C" "E" "D" "H" "S" "L")
     "There exists multiple cyclic paths between the competitors"))
   
   
   (limited-test-case
    "Test #3"
    ;power-ranking
    (check-equal?
     (power-ranking
      (list
       (defeated "A" "B")
       (tie "B" "C")
       (defeated "C" "D")
       (tie "D" "E")
       (defeated "E" "H")
       (tie "F" "I")
       (defeated "R" "K")
       (tie "S" "L")
       (defeated "T" "A")
       (tie "U" "B")
       (tie "V" "E")
       (defeated "W" "P")
       (tie "X" "B")
       (defeated "Y" "E")
       (tie "Z" "P")))
     (list "T" "Y" "W" "R" "A" "K" "F" "I" "L" "S" "Z"
           "P" "C" "U" "X" "B" "V" "E" "D" "H")
     "There exists multiple cyclic paths between the competitors"))
   
   
   (limited-test-case
    "Test #4"
    ;power-ranking
    (check-equal?
     (power-ranking
      (list
       (defeated "A" "B")
       (tie "B" "C")
       (defeated "C" "D")
       (tie "D" "E")
       (defeated "E" "H")
       (tie "F" "I")
       (tie "G" "K")
       (defeated "H" "L")
       (defeated "I" "M")
       (tie "J" "N")
       (defeated "K" "O")
       (tie "L" "P")
       (defeated "M" "K")
       (tie "N" "L")
       (defeated "O" "A")
       (tie "P" "B")
       (tie "C" "E")
       (defeated "J" "P")
       (tie "Q" "P")
       (defeated "R" "K")
       (tie "S" "L")
       (defeated "T" "A")
       (tie "U" "B")
       (tie "V" "E")
       (defeated "W" "P")
       (tie "X" "B")
       (defeated "Y" "E")
       (tie "Z" "P")))
     (list "R" "T" "W" "Y" "F" "I" "M" "G" "K" "O" "A" "C" "J"
           "N" "Q" "S" "U" "V" "X" "Z" "B" "E" "L" "P" "D" "H")
     "There exists multiple cyclic paths between the competitors"))
   
   
   (limited-test-case
    "Test #5"
    ;power-ranking
    (check-equal?
     (power-ranking
      (list
       (tie "B" "C")
       (tie "A" "B")))
     (list "A" "B" "C")
     "Alphabetical order gets the precedence"))
   
   
   (limited-test-case
    "Test #6"
    ;power-ranking
    (check-equal?
     (power-ranking
      (list
       (defeated "A" "B")
       (tie "B" "C")
       (tie "D" "E")
       (defeated "E" "H")
       (tie "F" "I")
       (defeated "H" "L")
       (defeated "I" "M")
       (tie "J" "N")
       (defeated "K" "O")
       (tie "L" "P")
       (defeated "M" "K")
       (tie "P" "B")
       (tie "C" "E")
       (tie "J" "P")))
     (list "A" "F" "I" "M" "K" "O" "C" "D" "E" "J" "N" "P" "B" "H" "L")
     "There exists multiple cyclic paths between the competitors"))
   
   
   (limited-test-case
    "Test #7"
    ;power-ranking
    (check-equal?
     (power-ranking
      (list
       (tie "A" "B")
       (tie "B" "C")
       (tie "C" "D")
       (tie "D" "E")
       (tie "E" "H")
       (tie "F" "I")
       (tie "G" "K")
       (tie "H" "L")
       (tie "I" "M")
       (tie "J" "N")
       (tie "K" "O")
       (tie "L" "P")
       (tie "M" "K")
       (tie "N" "L")
       (defeated "O" "A")
       (tie "P" "B")
       (tie "C" "E")
       (tie "J" "P")))
     (list "F" "G" "I" "K" "M" "O" "B" "C" "D" "E" "H" "J" "L" "N" "P" "A")
     "There exists multiple cyclic paths between the competitors"))
   
   
   (limited-test-case
    "Test #8"
    ;power-ranking
    (check-equal?
     (power-ranking
      (list
       (tie "A" "E")
       (defeated "C" "B")
       (defeated "B" "A")
       (defeated "A" "C")
       (tie "B" "C")))
     (list "E" "A" "B" "C")
     "E has the highest  non-losing percentage"))
   
   
   (limited-test-case
    "Test #9"
    ;power-ranking
    (check-equal?
     (power-ranking
      (list
       (defeated "C" "E")
       (defeated "D" "C")
       (tie "D" "B")))
     (list "B" "D" "C" "E")
     "Alphabetical order gets the precedence"))
   
   
   (limited-test-case
    "Test #10"
    ;power-ranking
    (check-equal?
     (power-ranking
      (list
       (defeated "A" "B")
       (defeated "B" "C")
       (defeated "C" "D")
       (defeated "D" "E")
       (tie "C" "E")))
     (list "A" "B" "C" "D" "E")
     "Alphabetical order gets the precedence"))
   
   
   (limited-test-case
    "Test #11"
    ;power-ranking
    (check-equal?
     (power-ranking
      (list
       (defeated "A" "B")
       (defeated "B" "C")
       (defeated "C" "D")
       (defeated "D" "E")
       (defeated "E" "H")
       (defeated "F" "I")
       (defeated "I" "M")
       (defeated "M" "K")
       (defeated "N" "L")
       (defeated "O" "A")
       (defeated "P" "B")
       (tie "C" "E")))
     (list "O" "P" "F" "N" "A" "I" "L" "M" "B" "K" "C" "E" "D" "H")
     "There exists multiple cyclic paths between the competitors"))
   
   
   (limited-test-case
    "Test #12"
    ;power-ranking
    (check-equal?
     (power-ranking
      (list
       (defeated "A" "B")
       (defeated "B" "C")
       (defeated "C" "D")
       (defeated "D" "E")
       (defeated "E" "H")
       (defeated "F" "I")
       (defeated "G" "K")
       (defeated "H" "L")
       (defeated "I" "M")
       (defeated "J" "N")
       (defeated "K" "O")
       (defeated "L" "P")
       (defeated "M" "K")
       (defeated "N" "L")
       (defeated "O" "A")
       (defeated "P" "B")
       (tie "C" "E")))
     (list "F" "G" "J" "I" "N" "M" "K" "O" "A" "C" "E" "D" "H" "P" "B" "L")
     "There exists multiple cyclic paths between the competitors"))
   
   
   (limited-test-case
    "Test #13"
    ;power-ranking
    (check-equal?
     (power-ranking
      (list
       (defeated "A" "B")
       (defeated "B" "C")
       (defeated "C" "D")
       (defeated "D" "E")
       (defeated "E" "H")
       (defeated "F" "I")
       (defeated "G" "K")
       (defeated "H" "L")
       (defeated "I" "M")
       (defeated "J" "N")
       (defeated "K" "O")
       (defeated "L" "P")
       (defeated "M" "K")
       (defeated "N" "L")
       (defeated "O" "A")
       (defeated "P" "B")
       (tie "C" "E")
       (tie "J" "P")))
     (list "F" "G" "I" "M" "K" "O" "A" "J" "C" "E" "P" "D" "H" "N" "B" "L")
     "There exists multiple cyclic paths between the competitors"))
   
   
   (limited-test-case
    "Test #14"
    ;power-ranking
    (check-equal?
     (power-ranking
      (list (defeated "A" "D")
            (defeated "A" "E")
            (defeated "C" "B")
            (defeated "C" "F")
            (tie "D" "B")
            (defeated "F" "E")))
     (list "C" "A" "F" "E" "B" "D")
     "C outranks 4"))
   
   
   (limited-test-case
    "Test #15"
    ;power-ranking
    (check-equal?
     (power-ranking
      (list
       (defeated "A" "B")
       (tie "B" "C")
       (defeated "C" "D")
       (tie "D" "E")
       (defeated "E" "H")
       (tie "F" "I")
       (tie "G" "K")
       (defeated "H" "L")
       (defeated "I" "M")
       (tie "J" "N")
       (defeated "K" "O")
       (tie "L" "P")
       (defeated "M" "K")
       (tie "N" "L")
       (defeated "O" "A")
       (tie "P" "B")
       (tie "C" "E")
       (tie "J" "P")))
     (list "F" "I" "M" "G" "K" "O" "A" "C" "E" "J" "N" "P" "B" "L" "D" "H")
     "There exists multiple cyclic paths between the competitors"))
   
   )
  )

(run-tests tests 'verbose)


