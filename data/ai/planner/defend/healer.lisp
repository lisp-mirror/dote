(in-package :goap)

(define-planner
  (:name :attack
         :preconditions         ((:weapon-loaded t)
                                 (:near-enemy    t))
         :effects               ((:curb-threat   t))
         :context-preconditions (enough-health-p none-needs-help-p !is-status-terror-p)
         :cost                  50)
  (:name :go-to-attack-pos
         :preconditions         ((:weapon-loaded             t)
                                 (:reach-with-current-weapon t))
         :effects               ((:near-enemy     t))
         :context-preconditions (!is-status-terror-p)
         :cost                  1)
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
         :cost                  10)
  (:name :protect-attack
         :preconditions         ((:protect-friend-attack t)
                                 (:weapon-loaded         t))
         :effects               ((:protecting  t))
         :context-preconditions (friend-needs-help-p)
         :cost                  20)
  (:name :protect-attack-spell
         :preconditions         ((:protect-friend-attack t))
         :effects               ((:protecting  t))
         :context-preconditions (friend-needs-help-p
                                 has-enough-sp-damage-p
                                 ;; "!is-status-terror-p"  is implicitly
                                 ;; managed by has-enough-sp-damage-p
                                 there-is-attackable-opponents-attack-spell-p)
         :cost                  5)
  (:name :go-near-weak-friend-attack
         :preconditions         ()
         :context-preconditions (can-minimally-move-p
                                 friend-needs-help-p
                                 can-attack-when-near-pos-p)
         :effects               ((:protect-friend-attack t))
         :cost                  20)
  (:name :go-near-weak-friend
         :preconditions         ()
         :context-preconditions (can-minimally-move-p
                                 friend-needs-help-p
                                 !can-attack-when-near-pos-p)
         :effects               ((:protecting t))
         :cost                  20)
  (:name :launch-heal-spell-friend
         :preconditions         ()
         :context-preconditions (has-enough-sp-heal-p
                                 friend-needs-help-p
                                 there-is-reachable-help-needed-friend-heal-spell-p)
                                 ;; "!is-status-terror-p"  is implicitly
                                 ;; managed by has-enough-sp-heal-p
          :effects                ((:protect-friend t))
          :cost                  20)

  (:name :launch-heal-spell
         :preconditions         ()
         :context-preconditions (has-enough-sp-heal-p
                                 someone-needs-help-p
                                 there-is-reachable-help-needed-friend-heal-spell-p)
                                 ;; "!is-status-terror-p"  is implicitly
                                 ;; managed by has-enough-sp-heal-p
         :effects               ((:curb-threat t))
         :cost                  5)
  (:name :launch-attack-spell
         :preconditions         ()
         :context-preconditions (none-needs-help-p
                                 has-enough-sp-damage-p
                                 ;; "!is-status-terror-p"  is implicitly
                                 ;; managed by has-enough-sp-damage-p
                                 there-is-attackable-opponents-attack-spell-p)
         :effects               ((:curb-threat t))
         :cost                  10)
  (:name :hide
         :preconditions         ((:has-hiding-place t))
         :context-preconditions (!enough-health-p is-visible-p)
         :effects               ((:curb-threat t))
         :cost                  15)
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
