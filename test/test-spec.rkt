#lang racket

(require redex "../src/spec.rkt")

(test-equal (redex-match? tic-tac-toe board (term ([_])))
            #t)
(test-equal (redex-match? tic-tac-toe board (term (_)))
            #f)
(test-equal (redex-match? tic-tac-toe board (term ([X X X] [O O O] [X _ X _] [O O O O X])))
            #t)

(test-equal (redex-match? tic-tac-toe board (term initial-board))
            #t)

(test-equal (flat-count "absolutely nuffin")
            0)
(test-equal (flat-count 0 '())
            0)
(test-equal (flat-count 0 '(1))
            0)
(test-equal (flat-count 0 0 0)
            2)
(test-equal (flat-count 0 '() '(0 1 0) '('(1 1) '(0 1 '(0 1 0))) 0)
            6)
(test-equal (flat-count (term X) (term initial-board))
            0)
(test-equal (flat-count (term X) (term ([X X X] [O O O] [X _ X _] [O O O O X])))
            6)

(test-results)