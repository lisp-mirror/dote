(in-package :goap)

(define-planner
  (:name :launch-attack-spell
         :preconditions         ((:near-enemy-attack-spell t))
         :context-preconditions (!disobey-1-out-100
                                 none-needs-help-p
                                 ;; "!is-status-terror-p"  is implicitly
                                 ;; managed by has-enough-sp-attack-p
                                 has-enough-sp-attack-p )
         :effects               ((:curb-threat t))
         :cost                  15)
  (:name :go-to-attack-spell-pos
         :preconditions         ()
         :effects               ((:near-enemy-attack-spell t))
         :context-preconditions (exists-reachable-pos-to-launch-attack-spell)
         :cost                  1)
  (:name :attack-spell-current-pos
         :preconditions         ()
         :effects               ((:near-enemy-attack-spell t))
         :context-preconditions (can-launch-attack-spell-current-pos-p)
         :cost                  1)
  (:name :damage-spell-current-pos
         :preconditions         ()
         :effects               ((:near-enemy-damage-spell t))
         :context-preconditions (can-launch-damage-spell-current-pos-p)
         :cost                  1)
  (:name :heal-spell-current-pos
         :preconditions         ()
         :effects               ((:near-enemy-heal-spell t))
         :context-preconditions (can-launch-heal-spell-current-pos-p)
         :cost                  1)
  (:name :launch-heal-spell
         :preconditions         ((:near-enemy-heal-spell t))
         :context-preconditions (has-enough-sp-heal-p
                                 ;; "!is-status-terror-p"  is implicitly
                                 ;; managed by has-enough-sp-heal-p
                                 someone-needs-help-p)
         :effects               ((:curb-threat t))
         :cost                  2)
  (:name :go-to-heal-spell-pos
         :preconditions         ()
         :effects               ((:near-enemy-heal-spell t))
         :context-preconditions (exists-reachable-pos-to-launch-heal-spell)
         :cost                  1)
  (:name :launch-damage-spell
         :preconditions         ((:near-enemy-damage-spell t))
         :context-preconditions (!disobey-1-out-100
                                 none-needs-help-p
                                 ;; "!is-status-terror-p"  is implicitly
                                 ;; managed by has-enough-sp-damage-p
                                 has-enough-sp-damage-p)
         :effects               ((:curb-threat t))
         :cost                  15)
  (:name :go-to-damage-spell-pos
         :preconditions         ()
         :effects               ((:near-enemy-damage-spell t))
         :context-preconditions (exists-reachable-pos-to-launch-damage-spell)
         :cost                  1)
  (:name :use-fountain
         :preconditions         ((:has-fountain-facing t))
         :context-preconditions (!enough-health-p pass-1d4)
         :effects               ((:curb-threat t))
         :cost                  10)
  (:name :go-to-fountain
         :preconditions         ((:has-fountain-near t))
         :context-preconditions ()
         :effects               ((:has-fountain-facing t))
         :cost                  1)
  (:name :find-fountain
         :preconditions         ()
         :context-preconditions (is-there-useful-reachable-fountain-p)
         :effects               ((:has-fountain-near t))
         :cost                  1)
  (:name :hide
         :preconditions         ((:has-hiding-place t))
         :context-preconditions (near-to-death-p is-visible-p)
         :effects               ((:curb-threat t))
         :cost                  5)
  (:name :find-hiding-place
         :preconditions         ()
         :context-preconditions (is-there-hiding-place-p)
         :effects               ((:has-hiding-place t))
         :cost                  1)
  (:name :rotate
         :preconditions         ()
         :effects               ((:curb-threat   t))
         :context-preconditions (can-rotate-p)
         :cost                  1000))
