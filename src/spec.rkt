#lang racket

(require redex)

(provide tic-tac-toe flat-count move initial-board)

(define-language tic-tac-toe
  [player ::= X O]
  [position ::= _ player]
  [board ::= ([position ...] ...)])

; flat-count consumes an item and some variable number of expressions
; and produces a number equal to the count of item in those expressions.
; The expressions are flattened, so they may be individual items,
; lists, lists of lists, etc.
(define (flat-count item . exprs)
  (count (curry equal? item) (flatten exprs)))

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

(define-term initial-board
  ([_ _ _]
   [_ _ _]
   [_ _ _]))