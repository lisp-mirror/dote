(in-package :goap)

(define-planner
  (:name :attack
         :preconditions         ((:weapon-loaded t)
                                 (:near-enemy    t))
         :effects               ((:curb-threat   t))
         :context-preconditions (enough-health-p no-friend-needs-help-p !is-status-terror-p)
         :cost                  1)
  (:name :go-to-attack-pos
         :preconditions         ((:weapon-loaded             t)
                                 (:reach-with-current-weapon t))
         :effects               ((:near-enemy     t))
         :context-preconditions (!is-status-terror-p)
         :cost                  1)
  (:name :go-near-to-attack-pos
         :preconditions         ()
         :effects               ((:curb-threat    t))
         :context-preconditions (enough-health-p
                                 no-friend-needs-help-p
                                 !is-status-terror-p
                                 exists-attack-goal-w-current-weapon-p
                                 !reachable-w-current-weapon-and-mp-p)
         :cost                  5)
  (:name :find-attack-position
         :preconditions         ((:weapon-loaded             t))
         :context-preconditions (exists-attack-goal-w-current-weapon-p
                                 reachable-and-attack-w-current-weapon-and-mp-p)
         :effects               ((:reach-with-current-weapon t))
         :cost                  1)
  (:name :load-weapon
         :preconditions         ()
         :context-preconditions (has-weapon-inventory-or-worn-p)
         :effects               ((:weapon-loaded t))
         :cost                  1))
