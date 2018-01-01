#lang racket
(require redex)

(define-language tic-tac-toe
  [position ::= _ X O]
  [board ::= ([position ...] ...)])

(module+ test
  (test-equal (redex-match? tic-tac-toe board (term ([_])))
              #t)
  (test-equal (redex-match? tic-tac-toe board (term (_)))
              #f)
  (test-equal (redex-match? tic-tac-toe board (term ([X X X] [O O O] [X _ X _] [O O O O X])))
              #t))

(define-term initial-board
  ([_ _ _]
   [_ _ _]
   [_ _ _]))