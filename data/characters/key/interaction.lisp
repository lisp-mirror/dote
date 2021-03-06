(in-package :random-key)

(define-interaction
    can-be-picked       true
    can-open            generate
    can-be-held-in-hand true
    decay               none
    effects             (define-effects)
    healing-effects     (define-healing-effects
			  heal-poison                  generate
			  heal-berserk                 generate
			  heal-faint                   generate
			  heal-terror                  generate
			  cause-poison                 generate
			  cause-berserk                generate
			  cause-faint                  generate
			  cause-terror                 generate
			  immune-poison                generate
			  immune-berserk               generate
			  immune-faint                 generate
			  immune-terror                generate)
    magic-effect          none)
