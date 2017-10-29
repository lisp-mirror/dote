(in-package :goap)

(define-planner
  (:name :move
         :effects               ((:curb-threat t))
         :context-preconditions (!disobey-1-out-100 enough-health-p able-to-move-p)
         :cost                  1)
  (:name :launch-teleport-spell
         :preconditions         ()
         :context-preconditions (has-enough-sp-teleport-p)
         :effects               ((:curb-threat t))
         :cost                  80)
  (:name :launch-heal-spell
         :preconditions         ()
         :context-preconditions (has-enough-sp-heal-p
                                 someone-needs-help-p
                                 there-is-reachable-help-needed-friend-heal-spell-p)
                                 ;; "!is-status-terror-p"  is implicitly
                                 ;; managed by has-enough-sp-heal-p
         :effects               ((:curb-threat t))
         :cost                  5))
