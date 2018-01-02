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

; flat-count consumes an item and some variable number of expressions
; and produces a number equal to the count of item in those expressions.
; The expressions are flattened, so they may be individual items,
; lists, lists of lists, etc.
(define (flat-count item . exprs)
  (count (curry equal? item) (flatten (append exprs))))

(module+ test
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
              6))