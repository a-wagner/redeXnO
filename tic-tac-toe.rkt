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

(module+ test
    (test-equal (redex-match? tic-tac-toe board (term initial-board))
                #t))

; flat-count consumes an item and some variable number of expressions
; and produces a number equal to the count of item in those expressions.
; The expressions are flattened, so they may be individual items,
; lists, lists of lists, etc.
(define (flat-count item . exprs)
  (count (curry equal? item) (flatten exprs)))

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

(define move
  (reduction-relation
   tic-tac-toe
   #:domain board
   (--> (any_1
         ...
         [any_2 ... _ any_3 ...]
         any_4
         ...)
        (any_1
         ...
         [any_2 ... X any_3 ...]
         any_4
         ...)
        (side-condition (= (flat-count (term X) (term (any_1 ...)) (term (any_2 ...)) (term (any_3 ...)) (term (any_4 ...)))
                           (flat-count (term O) (term (any_1 ...)) (term (any_2 ...)) (term (any_3 ...)) (term (any_4 ...)))))
        player-x)
   (--> (any_1
         ...
         [any_2 ... _ any_3 ...]
         any_4
         ...)
        (any_1
         ...
         [any_2 ... O any_3 ...]
         any_4
         ...)
        (side-condition (> (flat-count (term X) (term (any_1 ...)) (term (any_2 ...)) (term (any_3 ...)) (term (any_4 ...)))
                           (flat-count (term O) (term (any_1 ...)) (term (any_2 ...)) (term (any_3 ...)) (term (any_4 ...)))))
        player-o)))

; consumes a player and produces a board
; in which that player wins 3 across in the
; top row
(define (horiz-win player)
  (term ([,player ,player ,player]
         [_ _ _]
         [_ _ _])))

; consumes a player and produces a board
; in which that player wins 3 down in the
; leftmost column
(define (vert-win player)
  (term ([,player _ _]
         [,player _ _]
         [,player _ _])))

; consumes a player and produces a board
; in which that player wins 3 diagonal from
; top left to bottom right
(define (diag-lr-win player)
  (term ([,player _ _]
         [_ ,player _]
         [_ _ ,player])))

; consumes a player and produces a board
; in which that player wins 3 diagonal from
; top right to bottom left
(define (diag-rl-win player)
  (term ([_ _ ,player]
         [_ ,player _]
         [,player _ _])))

(module+ test
  (test-equal (redex-match? tic-tac-toe board (horiz-win (term X)))
              #t)
  (test-equal (redex-match? tic-tac-toe board (vert-win (term O)))
              #t)
  (test-equal (redex-match? tic-tac-toe board (diag-lr-win (term O)))
              #t)
  (test-equal (redex-match? tic-tac-toe board (diag-rl-win (term X)))
              #t))

                      