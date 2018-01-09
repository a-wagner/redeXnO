#lang racket

(require redex "../src/pred.rkt" "../src/spec.rkt" "../src/win-gen.rkt")

(test-equal (win? (term initial-board))
            #f)
(test-equal (win? (horiz-win (term X)))
            #t)
(test-equal (win? (vert-win (term X)))
            #t)
(test-equal (win? (diag-lr-win (term X)))
            #t)
(test-equal (win? (diag-rl-win (term X)))
            #t)
(test-equal (win? (horiz-win (term X)) (term X))
            #t)
(test-equal (win? (vert-win (term X)) (term X))
            #t)
(test-equal (win? (diag-lr-win (term X)) (term X))
            #t)
(test-equal (win? (diag-rl-win (term X)) (term X))
            #t)
(test-equal (win? (horiz-win (term X)) (term O))
            #f)
(test-equal (win? (vert-win (term X)) (term O))
            #f)
(test-equal (win? (diag-lr-win (term X)) (term O))
            #f)
(test-equal (win? (diag-rl-win (term X)) (term O))
            #f)
(test-equal (win? (term initial-board))
            #f)
(test-equal (win? (horiz-win (term O)))
            #t)
(test-equal (win? (vert-win (term O)))
            #t)
(test-equal (win? (diag-lr-win (term O)))
            #t)
(test-equal (win? (diag-rl-win (term O)))
            #t)

(test-equal (valid? (term initial-board))
            #t)
(test-equal (valid? (term ([X _ _] [_ _ _] [_ _ _])))
            #t)
(test-equal (valid? (term ([O _ _] [_ _ _] [_ _ _])))
            #f)
(test-equal (valid? (term ([X _ _] [O _ _] [_ _ O])))
            #f)

(test-equal (isomorphic? (term initial-board) (term initial-board))
            #t)
(test-equal (isomorphic? (term initial-board) (term X))
            #f)
(test-equal (isomorphic? (term initial-board) (horiz-win (term X)))
            #f)
(test-equal (isomorphic? (horiz-win (term X)) (horiz-win (term X)))
            #t)
(test-equal (isomorphic? (horiz-win (term X)) (vert-win (term X)))
            #t)
(test-equal (isomorphic? (horiz-win (term X)) (horiz-win (term O)))
            #f)
(test-equal (isomorphic? (diag-lr-win (term O)) (diag-rl-win (term O)))
            #t)

(test-results)