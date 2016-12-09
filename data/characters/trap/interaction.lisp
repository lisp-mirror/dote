(in-package :random-trap)

(define-interaction
    can-be-picked       true
    can-be-dropped      true
    can-attack          false
    decay               generate
    effects          (define-effects)
    healing-effects  (define-healing-effects)
    magic-effect generate)
