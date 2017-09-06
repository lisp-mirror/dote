(in-package :goap)

(define-planner
  (:name :attack
         :preconditions         ((:weapon-loaded t)
                                 (:near-enemy    t))
         :effects               ((:curb-threat   t))
         :context-preconditions (enough-health-p no-friend-needs-help-p !is-status-terror-p)
         :cost                  1)
  (:name :protect
         :preconditions         ((:protect-friend t))
         :effects               ((:curb-threat    t))
         :context-preconditions (friend-needs-help-p)
         :cost                  10)
  (:name :go-near-to-attack-pos
         :preconditions         ()
         :effects               ((:curb-threat    t))
         :context-preconditions (enough-health-p
                                 no-friend-needs-help-p
                                 !is-status-terror-p
                                 exists-attack-goal-w-current-weapon-p
                                 !reachable-w-current-weapon-and-mp-p)
         :cost                  5)
  (:name :go-to-attack-pos
         :preconditions         ((:weapon-loaded             t)
                                 (:reach-with-current-weapon t))
         :effects               ((:near-enemy     t))
         :context-preconditions (!is-status-terror-p)
         :cost                  1)
  (:name :find-attack-position
         :preconditions         ((:weapon-loaded             t))
         :context-preconditions (exists-attack-goal-w-current-weapon-p
                                 reachable-w-current-weapon-and-mp-p)
         :effects               ((:reach-with-current-weapon t))
         :cost                  1)
  (:name :load-weapon
         :preconditions         ()
         :context-preconditions (has-weapon-inventory-or-worn-p)
         :effects               ((:weapon-loaded t))
         :cost                  1)
  (:name :launch-spell
         :preconditions         ()
         :context-preconditions (has-enough-sp-damage-p)
         :effects               ((:curb-threat t))
         :cost                  50)
  (:name :flee
         :preconditions         ((:has-escape t))
         :effects               ((:curb-threat t))
         :cost                  20)
  (:name :find-way-escape
         :preconditions         ()
         :context-preconditions (is-there-escape-way-p) ; TODO
         :effects               ((:has-escape t))
         :cost                  5)
  (:name :launch-teleport-spell
         :preconditions         ()
         :context-preconditions (has-enough-sp-teleport-p)
         :effects               ((:has-escape t))
         :cost                  8)
  (:name :launch-spell-wall
         :preconditions         ()
         :context-preconditions (has-enough-sp-break-wall-p)
         :effects               ((:has-escape t))
         :cost                  10)
  (:name :block-way-to-weak-friend
         :preconditions         ()
         :context-preconditions (friend-needs-help-p)
         :effects               ((:protect-friend t))
         :cost                  5)
  (:name :launch-heal-spell
         :preconditions         ()
         :context-preconditions (friend-needs-help-p has-enough-sp-heal-p)
         :effects               ((:protect-friend t))
         :cost                  5))
