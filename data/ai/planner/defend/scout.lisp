(in-package :goap)

(define-planner
  (:name :attack
         :preconditions         ((:weapon-loaded t)
                                 (:near-enemy    t))
         :effects               ((:curb-threat   t))
         :context-preconditions (enough-health-p none-needs-help-p !is-status-terror-p)
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
         :context-preconditions (can-minimally-move-p
                                 enough-health-p
                                 none-needs-help-p
                                 !is-status-terror-p
                                 !is-in-attack-pos-p
                                 exists-attack-goal-w-current-weapon-p
                                 !reachable-opt/path-attack-current-weapon-and-mp
                                 can-move-near-attack-pos)
         :cost                  5)
  (:name :find-attack-position
         :preconditions         ((:weapon-loaded             t))
         :context-preconditions (exists-attack-goal-w-current-weapon-p
                                 reachable-opt/path-attack-current-weapon-and-mp)
         :effects               ((:reach-with-current-weapon t))
         :cost                  1)
  (:name :load-weapon
         :preconditions         ()
         :context-preconditions (has-weapon-inventory-or-worn-p)
         :effects               ((:weapon-loaded t))
         :cost                  1)
  (:name :protect
         :preconditions         ((:protecting t))
         :effects               ((:curb-threat    t))
         :context-preconditions (friend-needs-help-p)
         :cost                  1)
  (:name :protect-attack
         :preconditions         ((:protect-friend-attack t)
                                 (:weapon-loaded         t))
         :effects               ((:protecting  t))
         :context-preconditions (friend-needs-help-p)
         :cost                  10)
  (:name :go-near-weak-friend-attack
         :preconditions         ()
         :context-preconditions (can-minimally-move-p
                                 friend-needs-help-p
                                 can-attack-when-near-pos-p)
         :effects               ((:protect-friend-attack t))
         :cost                  5)
  (:name :go-near-weak-friend
         :preconditions         ()
         :context-preconditions (can-minimally-move-p
                                 friend-needs-help-p
                                 !can-attack-when-near-pos-p)
         :effects               ((:protecting t))
         :cost                  5)
  (:name :launch-heal-spell
         :preconditions         ((:near-enemy-heal-spell t))
         :context-preconditions (has-enough-sp-heal-p
                                 ;; "!is-status-terror-p"  is implicitly
                                 ;; managed by has-enough-sp-heal-p
                                 someone-needs-help-p)
         :effects               ((:curb-threat t))
         :cost                  20)
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
         :cost                  2)
  (:name :go-to-damage-spell-pos
         :preconditions         ()
         :effects               ((:near-enemy-damage-spell t))
         :context-preconditions (exists-reachable-pos-to-launch-damage-spell)
         :cost                  1)
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
  (:name :launch-attack-spell
         :preconditions         ()
         :context-preconditions (none-needs-help-p
                                 has-enough-sp-attack-p
                                 ;; "!is-status-terror-p"  is implicitly
                                 ;; managed by has-enough-sp-attack-p
                                 there-is-attackable-opponents-attack-spell-p)
         :effects               ((:curb-threat t))
         :cost                  20)
  (:name :hide
         :preconditions         ((:has-hiding-place t))
         :context-preconditions (near-to-death-p is-visible-p)
         :effects               ((:curb-threat t))
         :cost                  10)
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
