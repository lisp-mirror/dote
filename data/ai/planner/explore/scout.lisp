(in-package :goap)

(define-planner
  (:name :move
         :effects               ((:curb-threat t))
         :context-preconditions (!disobey-1-out-10 !near-to-death-p able-to-explore-p)
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
         :cost                  5)
  (:name :use-fountain
         :preconditions         ((:has-fountain-facing t))
         :context-preconditions (!enough-health-p pass-1d4)
         :effects               ((:curb-threat t))
         :cost                  1)
  (:name :go-to-fountain
         :preconditions         ((:has-fountain-near t))
         :context-preconditions ()
         :effects               ((:has-fountain-facing t))
         :cost                  1)
  (:name :find-fountain
         :preconditions         ()
         :context-preconditions (is-there-useful-reachable-fountain-p)
         :effects               ((:has-fountain-near t))
         :cost                  1))
