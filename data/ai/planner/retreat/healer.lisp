(in-package :goap)

(define-planner
  (:name :flee
         :preconditions         ()
         :context-preconditions (!is-visible-p is-able-to-flee-p)
         :effects               ((:curb-threat t))
         :cost                  5)
  (:name :hide
         :preconditions         ((:has-hiding-place t))
         :context-preconditions (is-visible-p)
         :effects               ((:curb-threat t))
         :cost                  1)
  (:name :place-trap
         :preconditions         ()
         :context-preconditions (has-trap-p can-place-trap-p pass-1d10)
         :effects               ((:curb-threat t))
         :cost                  5)
  (:name :find-hiding-place
         :preconditions         ()
         :context-preconditions (is-there-hiding-place-p)
         :effects               ((:has-hiding-place t))
         :cost                  1)
  (:name :launch-teleport-spell
         :preconditions         ()
         :context-preconditions (!disobey-1-out-100
                                 has-enough-sp-teleport-p
                                 is-visible-p)
         :effects               ((:curb-threat t))
         :cost                  80)
  (:name :launch-spell-wall
         :preconditions         ()
         :context-preconditions (has-enough-sp-break-wall-p is-visible-p
                                 has-wall-near-p)
         :effects               ((:has-hiding-place t))
         :cost                  100)
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
         :cost                  1))
