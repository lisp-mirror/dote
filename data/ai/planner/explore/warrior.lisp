(in-package :goap)

(define-planner
  (:name :move
         :effects               ((:curb-threat t))
         :context-preconditions (enough-health-p !disobey-1-out-100)
         :cost                  1)
  (:name :use-teleport
         :preconditions         ()
         :context-preconditions (has-enough-sp-teleport-p)
         :effects               ((:curb-threat t))
         :cost                  80)
  (:name :launch-cure-spell
         :preconditions         ()
         :context-preconditions (has-enough-sp-heal-p)
         :effects               ((:curb-threat t))
         :cost                  5)
  (:name :idle
         :preconditions         ()
         :context-preconditions ()
         :effects               ((:curb-threat t))
         :cost                  100000))
