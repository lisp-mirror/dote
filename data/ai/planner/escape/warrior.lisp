(in-package :goap)

(define-planner
  (:name :flee
         :preconditions         ((:has-escape t))
         :effects               ((:curb-threat t))
         :cost                  1)
  (:name :find-way-escape
         :preconditions         ()
         :context-preconditions (is-there-escape-way-p)
         :effects               ((:has-escape t))
         :cost                  1)
  (:name :launch-teleport-spell
         :preconditions         ()
         :context-preconditions (has-enough-sp-teleport-p)
         :effects               ((:curb-threat t))
         :cost                  80)
  (:name :launch-spell-wall
         :preconditions         ()
         :context-preconditions (has-enough-sp-break-wall-p)
         :effects               ((:has-escape t))
         :cost                  5))
