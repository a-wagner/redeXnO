#lang racket

(require redex)

(provide horiz-win vert-win diag-lr-win diag-rl-win)

; consumes a player term and produces a board term
; in which that player wins 3 across in the
; top row
(define (horiz-win player)
  (term ([,player ,player ,player]
         [_ _ _]
         [_ _ _])))

; consumes a player term and produces a board term
; in which that player wins 3 down in the
; leftmost column
(define (vert-win player)
  (term ([,player _ _]
         [,player _ _]
         [,player _ _])))

; consumes a player term and produces a board term
; in which that player wins 3 diagonal from
; top left to bottom right
(define (diag-lr-win player)
  (term ([,player _ _]
         [_ ,player _]
         [_ _ ,player])))

; consumes a player term and produces a board term
; in which that player wins 3 diagonal from
; top right to bottom left
(define (diag-rl-win player)
  (term ([_ _ ,player]
         [_ ,player _]
         [,player _ _])))