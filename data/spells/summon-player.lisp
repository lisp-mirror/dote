(in-package :spell)

(define-spell (:summon-pc-1)
  :level                 5
  :element               :fire
  :tags                  (:invisible-to-player :heal :summon-player)
  :description           "Should be used by fountains only."
  :gui-texture           "misc/teleport-2.tga"
  :cost                  20.0
  :visual-effect-self    none
  :range                 1  ;; in tile units
  :effective-range       0  ;; in tile units
  :visual-effect-target  particles:make-summon-player-fx
  :effects
  (lambda (attacker defender)
    (declare (ignore attacker))
    (with-accessors ((state state)) defender
      (game-state:with-world (world state)
         (mtree:add-child (world:gui world)
                          (widget:make-player-generator world 1))))))
