(in-package :goap)

(define-planner
  (:name :launch-attack-spell
         :preconditions         ()
         :context-preconditions (none-needs-help-p
                                 has-enough-sp-attack-p
                                 ;; "!is-status-terror-p"  is implicitly
                                 ;; managed by has-enough-sp-attack-p
                                 there-is-attackable-opponents-attack-spell-p)
         :effects               ((:curb-threat t))
         :cost                  2)
  (:name :protect
         :preconditions         ((:protecting t))
         :effects               ((:curb-threat    t))
         :context-preconditions (friend-needs-help-p)
         :cost                  1)
  (:name :protect-attack-spell
         :preconditions         ((:protect-friend-attack-spell t))
         :effects               ((:protecting  t))
         :context-preconditions (friend-needs-help-p
                                 ;; "!is-status-terror-p"  is implicitly
                                 ;; managed by has-enough-sp-attack-p
                                 has-enough-sp-attack-p)
         :cost                   5)
  (:name :go-near-weak-friend
         :preconditions         ()
         :context-preconditions (can-minimally-move-p
                                 friend-needs-help-p
                                 !can-attack-when-near-pos-p)
         :effects               ((:protect-friend-attack-spell t))
         :cost                  5)
  (:name :launch-heal-spell-friend
         :preconditions         ()
         :context-preconditions (has-enough-sp-heal-p
                                 friend-needs-help-p
                                 there-is-reachable-help-needed-friend-heal-spell-p)
                                 ;; "!is-status-terror-p"  is implicitly
                                 ;; managed by has-enough-sp-heal-p
         :effects                ((:protect-friend t))
         :cost                  20)
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
  (:name :launch-heal-spell
         :preconditions         ()
         :context-preconditions (has-enough-sp-heal-p
                                 someone-needs-help-p
                                 there-is-reachable-help-needed-friend-heal-spell-p)
                                 ;; "!is-status-terror-p"  is implicitly
                                 ;; managed by has-enough-sp-heal-p
         :effects               ((:curb-threat t))
         :cost                  15)
  (:name :hide
         :preconditions         ((:has-hiding-place t))
         :context-preconditions (!enough-health-p is-visible-p)
         :effects               ((:curb-threat t))
         :cost                  5)
  (:name :find-hiding-place
         :preconditions         ()
         :context-preconditions (is-there-hiding-place-p)
         :effects               ((:has-hiding-place t))
         :cost                  1)
  (:name :launch-teleport-spell
         :preconditions         ()
         :context-preconditions (!enough-health-p has-enough-sp-teleport-p is-visible-p)
         :effects               ((:curb-threat t))
         :cost                  20)
  (:name :rotate
         :preconditions         ()
         :effects               ((:curb-threat   t))
         :context-preconditions (can-rotate-p)
         :cost                  1000))
