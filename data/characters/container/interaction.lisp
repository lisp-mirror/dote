(in-package :random-container)

(define-interaction
    can-be-picked       false
    can-talk            false
    can-ask-for-help    false
    can-be-opened       generate
    can-open            false
    can-attack          false
    can-be-attacked     false
    can-be-destroyed    true
    can-be-burned       true
    can-heal            false
    can-be-heal         false
    can-poison          false
    can-be-poisoned     false
    can-be-drunk        false
    can-be-eaten        false
    can-be-worn-arm     false
    can-be-worn-head    false
    can-be-worn-neck    false
    can-be-worn-feet    false
    can-cut             false
    can-smash           false
    can-pierce          false
    can-launch-bolt     false
    can-launch-arrow    false
    mounted-on-pole     false
    decay               none
    effects             (define-effects)
    healing-effects     (define-healing-effects
			  heal-poison                generate
			  heal-berserk               generate
			  heal-terror                generate
			  cause-poison               generate
			  cause-berserk              generate
			  cause-faint                generate
			  cause-terror               generate)
    magic-effect        none)
