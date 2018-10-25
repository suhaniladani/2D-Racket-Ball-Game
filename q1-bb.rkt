#lang racket
(require racket/sandbox)
(require rackunit)
(require rackunit/text-ui)
(require "q1.rkt")



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



;; test case
(define tests
  (test-suite
   "q1"
   
   (limited-test-case
    "Test #1"
    ;defeated?
    (check-true
     (defeated? "A" "B" (list
                         (tie "A" "B")
                         (defeated "B" "A")
                         (tie "B" "C")))
     "Should retrun True since there is tie between A and B"))
   
   
   (limited-test-case
    "Test #2"
    ;defeated?
    (check-false
     (defeated? "A" "B" (list
                         (tie "A" "E")
                         (defeated "C" "B")
                         (defeated "B" "A")
                         (defeated "A" "C")
                         (tie "B" "C")))
     "Should return false since defeat is not transitive"))
   
   
   (limited-test-case
    "Test #3"
    ;outranks
    (check-equal?
     (remove-duplicates
     (outranks "A"
               (list
                (defeated "C" "B")
                (defeated "B" "A")
                (defeated "A" "C")
                (tie "B" "C"))))
     (list "A" "B" "C")
     "There exists a cyclic path between the competitors"))
   
   
   (limited-test-case
    "Test #4"
    ;outranks
    (check-equal?
     (remove-duplicates
     (outranks "A"
               (list
                (defeated "C" "B")
                (defeated "A" "C")
                (tie "B" "D"))))
     (list "B" "C" "D")
     "A outranks B and D transitively"))
   
   
   (limited-test-case
    "Test #5"
    ;outranks
    (check-true
     (empty? (outranks "E"
                       (list
                        (defeated "C" "E")
                        (defeated "B" "C")
                        (tie "B" "D")
                        (tie "D" "B"))))
     "E does not outrank anyone, hence list should be empty"))
   
   
   (limited-test-case
    "Test #6"
    ;outranks
    (check-equal?
     (remove-duplicates
     (outranks "A"
               (list
                (defeated "A" "B")
                (defeated "B" "C")
                (defeated "C" "D")
                (defeated "D" "E")
                (tie "A" "E"))))
     (list "A" "B" "C" "D" "E")
     "There exists a cyclic path between the competitors"))
   
   
   (limited-test-case
    "Test #7"
    ;outranked-by
    (check-equal?
     (remove-duplicates
     (outranked-by  "C"
                    (list
                     (defeated "A" "B")
                     (defeated "B" "C")
                     (defeated "C" "D")
                     (defeated "D" "E")
                     (tie "C" "E"))))
     (list "A" "B" "C" "D" "E")
     "There exists a cyclic path between C and E"))
   
   
   (limited-test-case
    "Test #8"
    ;outranked-by
    (check-true
     (empty?
      (outranked-by  "A"
                     (list
                      (defeated "A" "B")
                      (defeated "B" "C")
                      (defeated "C" "D")
                      (defeated "D" "E")
                      (tie "C" "E"))))
     "A never gets defeated or tied, hence list should be empty"))
   
   
   (limited-test-case
    "Test #9"
    ;outranked-by
    (check-true
     (empty?
      (outranked-by "F"
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
                     (tie "C" "E"))))
     "F never gets defeated or tied, hence list should be empty"))
   
   
   
   (limited-test-case
    "Test #10"
    ;outranks
    (check-equal?
     (remove-duplicates
     (outranks "F"
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
                (tie "C" "E"))))
     (list "A" "B" "C" "D" "E" "H" "I" "K" "L" "M" "O" "P")
     "There exists multiple cyclic paths between the competitors"))
   
   
   (limited-test-case
    "Test #11"
    ;outranked-by
    (check-equal?
     (remove-duplicates
     (outranked-by "E"
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
                    (tie "J" "P"))))
     (list "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P")
     "There exists multiple cyclic paths between the competitors"))
   
   (limited-test-case
    "Test #12"
    ;outranks
    (check-equal?
     (remove-duplicates
     (outranks "E"
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
                (tie "J" "P"))))
     (list "B" "C" "D" "E" "H" "J" "L" "N" "P")
     "There exists multiple cyclic paths between the competitors"))
   
   
   (limited-test-case
    "Test #13"
    ;outranked-by
    (check-equal?
     (remove-duplicates
     (outranked-by "F"
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
                    (tie "J" "P"))))
     (list "F" "I")
     "F is outranked by F because it ties with I"))
   
   
   (limited-test-case
    "Test #14"
    ;outranks
    (check-equal?
     (remove-duplicates
     (outranks "F"
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
                (tie "J" "P"))))
     (list "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P")
     "There exists multiple cyclic paths between the competitors"))
   
   
   (limited-test-case
    "Test #15"
    ;defeated?
    (check-true
     (defeated? "A" "B" (list
                         (defeated "A" "B")
                         (defeated "B" "A")
                         (tie "B" "C")))
     "Should return true"))
   
   
   (limited-test-case
    "Test #16"
    ;outranks
    (check-equal?
     (remove-duplicates
     (outranks "E"
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
                (tie "J" "P"))))
     (list "B" "C" "D" "E" "H" "J" "L" "N" "P")
     "There exists multiple cyclic paths between the competitors"))
   
   
   
   (limited-test-case
    "Test #17"
    ;defeated?
    (check-false
     (defeated? "A" "C" (list
                         (tie "A" "B")
                         (defeated "B" "A")
                         (tie "B" "C")))
     "Should return false since defeat is not transitive"))
   
   )
  )


(run-tests tests 'verbose)


