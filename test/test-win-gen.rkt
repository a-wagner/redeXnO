#lang racket

(require redex "../src/win-gen.rkt" "../src/spec.rkt")

(test-equal (redex-match? tic-tac-toe board (horiz-win (term X)))
            #t)
(test-equal (redex-match? tic-tac-toe board (vert-win (term O)))
            #t)
(test-equal (redex-match? tic-tac-toe board (diag-lr-win (term O)))
            #t)
(test-equal (redex-match? tic-tac-toe board (diag-rl-win (term X)))
            #t)

(test-results)