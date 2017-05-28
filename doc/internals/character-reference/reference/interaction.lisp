(define-interaction
    can-talk            true
    can-ask-for-help    "Help!"
    can-be-opened       false  ; or string code
    can-open            "1234" ; or false
    can-attack          false
    can-intercept-attacks false
    can-be-attacked     true
    can-be-destroyed    true
    can-be-burned       true
    can-heal            false
    can-be-heal         false
    can-poison          false
    can-be-poisoned     false
    can-be-drunk        false
    can-be-eaten        false
    can-be-worn-arm   false
    can-be-worn-head  false
    can-be-worn-neck  false
    can-be-worn-feet    false
    can-be-worn-body    false
    can-be-worn-hand    false
    can-be-held-in-hand false
    can-be-picked        false
    can-be-dropped      false
    can-cut             true
    can-smash           false
    can-pierce          false
    can-launch-bolt      false
    can-launch-arrow     false
    mounted-on-pole     true
    decay
    (define-decay (points 10 duration turns message  "tree disappeared"))
    effects
    (define-effects
      ;;;;;;;;;;;;;;;; artifact
      strength                   (define-effect
				       (modifier 1 duration unlimited trigger when-used))
      stamina                      (define-effect
				       (modifier 2 duration unlimited trigger when-consumed))
      dexterity                     (define-effect
					(modifier -4 duration 3 ;; turns
                                                  trigger when-worn))
      agility                       (define-effect
					(modifier -4 duration unlimited trigger when-used))
      smartness                    none
      empaty                       none
      weight                       none
      damage-points                 none
      movement-points               none
      magic-points                  none
      dodge-chance                 none
      melee-attack-chance          none
      range-attack-chance          none
      melee-attack-damage          none
      range-attack-damage          none
      ;;;;;;;;;;;;;;;;;;
      edge-weapons-chance-bonus    none
      edge-weapons-damage-bonus    none
      impact-weapons-chance-bonus  none
      impact-weapons-damage-bonus  none
      pole-weapons-chance-bonus    none
      pole-weapons-damage-bonus    none
      range-weapons-chance-bonus    none
      range-weapons-damage-bonus    none
      unlock-chance                none
      deactivate-trap-chance       none
      reply-attack-chance          none
      ambush-attack-chance         none
      spell-chance                 none
      attack-spell-chance          none)
    healing-effects
    (define-healing-effects
	heal-damage-points           (define-heal-dmg-effect (duration unlimited
							      points    10.0
				                            trigger when-used
							    chance 0.1
						            target other))
      heal-poison                  (define-healing-effect  (duration unlimited
							    trigger  when-used
							    chance   0.2
							    target   self))
      heal-berserk                 none
      heal-faint                   none
      heal-terror                  none
      cause-poison                 (define-poison-effect (points -10
							  trigger when-used
							  target self
							  chance 0.3))
      cause-berserk                none
      cause-faint                  none
      cause-terror                 none
      immune-poison                (define-healing-effect (duration unlimited
							   trigger when-used
							   chance 0.1)) ;; if
									;; no
									;; target
									;; is
									;; given
									;; 'self'
									;; is
									;; assumed
      immune-berserk               none
      immune-faint                 none
      immune-terror                none)
    magic-effect
    (define-magic-effect (spell-id fireball-1 trigger when-used target other)))
