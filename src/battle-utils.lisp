;; dawn of the Era: a tactical game.
;; Copyright (C) 2015  cage

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(in-package :battle-utils)

(define-constant +recover-from-faint-dmg-fraction+ 0.25 :test #'=)

(define-constant +attack-min-sigma+               20.0  :test #'=)

(define-constant +attack-max-sigma+               200.0  :test #'=)

(define-constant +weapon-combination-bonus+        0.1  :test #'=)

(define-constant +weapon-pole-range+                 2  :test #'=)

(define-constant +weapon-melee-range+                1  :test #'=)

(defun 2d-gaussian (x y sigma h)
  (d* h (dexp (d- (d+ (d/ (dexpt x 2.0) (d* 2.0 (dexpt sigma 2.0)))
		      (d/ (dexpt y 2.0) (d* 2.0 (dexpt sigma 2.0))))))))

(defun standard-battle-gaussian-fn (height sigma)
  #'(lambda (x y)
      (2d-gaussian x y sigma height)))

(defun calc-attack-sigma (weapon-level attack-dmg &optional (shield-level nil) (armour-level nil))
  (d+ (dlerp (smoothstep-interpolate 0.0 1.0 (d/ (d+ weapon-level (d* attack-dmg 0.06))
						 10.0))
	     +attack-min-sigma+ +attack-max-sigma+)
      (d- (if shield-level
	      (d* 4.0 shield-level)
	      0.0))
      (d- (if armour-level
	      (d* 4.0 armour-level)
	      0.0))))

(defun attack-gaussian-fn (attack-dmg bonus-attack-dmg weapon-level
			   &optional (shield-level nil) (armor-level nil))
  (let ((sigma      (calc-attack-sigma weapon-level attack-dmg shield-level armor-level))
	(max-damage (d+ attack-dmg bonus-attack-dmg)))
    (standard-battle-gaussian-fn max-damage sigma)))

(defun attack-damage (attack-dmg    bonus-attack-dmg
		      attack-chance bonus-attack-chance
		      weapon-level
		      dodge-chance
		      &optional
			(shield-level nil)
			(armor-level  nil)
			(ambush       nil))
  (if (pass-d100.0 (max (d- (d+ attack-chance bonus-attack-chance)
			    dodge-chance)
			0.0))
      (let* ((actual-shield-level (and shield-level
				       (if ambush
					   (max 0.0 (d- shield-level 1.0))
					   shield-level)))
	     (actual-armor-level (and armor-level
				      (if ambush
					  (max 0.0 (d- armor-level 1.0))
					  armor-level)))
	     (gaussian-fn (attack-gaussian-fn attack-dmg
					      bonus-attack-dmg
					      weapon-level
					      actual-shield-level
					      actual-armor-level)))
	(funcall gaussian-fn
		 (d- 100.0 (lcg-next-upto 100.0))   ; attacker
		 (d- 100.0 (lcg-next-upto 100.0)))) ; defender
      nil))

(defun attack-spell-damage (attack-dmg dodge-chance
			    &optional
			      (shield-level nil)
			      (armor-level  nil)
			      (ambush       nil))
  (if (not (pass-d100.0 dodge-chance))
      (let* ((actual-shield-level (and shield-level
				       (if ambush
					   (max 0.0 (d- shield-level 1.0))
					   shield-level)))
	     (actual-armor-level (and armor-level
				      (if ambush
					  (max 0.0 (d- armor-level 1.0))
					  armor-level)))
	     (gaussian-fn (attack-gaussian-fn attack-dmg
					      0.0
					      9.0
					      actual-shield-level
					      actual-armor-level)))
	(funcall gaussian-fn
		 (d- 100.0 (lcg-next-upto 100.0))   ; attacker
		 (d- 100.0 (lcg-next-upto 100.0)))) ; defender
      nil))

(defun dump-gaussian (producer-fn)
  (loop for x from -0.0 below 100.0 by 1.0 do
       (loop for y from -0.0 below 100.0 by 1.0 do
	    (format t "~a ~a ~a~%" x y (funcall producer-fn x y)))))

(defgeneric bonus-attack-weapon-combination (weapon-atk weapon-def))

(defmethod bonus-attack-weapon-combination (weapon-atk weapon-def)
  0.0)

(defmethod bonus-attack-weapon-combination ((weapon-atk (eql :impact)) (weapon-def (eql :edge)))
  +weapon-combination-bonus+)

(defmethod bonus-attack-weapon-combination ((weapon-atk (eql :edge)) (weapon-def (eql :bow)))
  +weapon-combination-bonus+)

(defmethod bonus-attack-weapon-combination ((weapon-atk (eql :edge)) (weapon-def (eql :crossbow)))
  +weapon-combination-bonus+)

(defmethod bonus-attack-weapon-combination ((weapon-atk (eql :bow)) (weapon-def (eql :impact)))
  +weapon-combination-bonus+)

(defmethod bonus-attack-weapon-combination ((weapon-atk (eql :crossbow)) (weapon-def (eql :impact)))
  +weapon-combination-bonus+)

(defgeneric bonus-defense-weapon-combination (weapon-atk weapon-def))

(defmethod bonus-defense-weapon-combination (weapon-atk weapon-def)
  0.0)

(defmethod bonus-defense-weapon-combination (weapon-atk (weapon-def (eql :pole)))
  (d- +weapon-combination-bonus+))

(defun defend-from-container-trap (opened opener)
  (send-effects-after-attack opened opener :weapon (entity:ghost opened)))

(defun defend-from-fountain-interaction (fountain player)
  (send-effects-after-attack fountain player :weapon (entity:ghost fountain)))

(defun send-effects-after-attack (attacker defender
				  &key (weapon (character:worn-weapon (entity:ghost attacker))))
  (let* ((effects-to-others (remove-if
			     #'(lambda (a)
				 (and (funcall ;; only "when-"used" triggered effects
				       (random-object-messages:untrigged-effect-p-fn
					basic-interaction-parameters:+effect-when-used+)
				       a)
				      (funcall ;; only effects affecting others
				       (random-object-messages:to-other-target-effect-p-fn)
				       a)))
			     (random-object-messages:params->effects-messages
			      weapon))))
    (random-object-messages:propagate-effects-msg weapon defender effects-to-others)))

(defun send-attack-melee-event (attacker defender)
  (let* ((weapon-type  (character:weapon-type-short-range (entity:ghost attacker)))
	 (max-dist-atk (if (eq weapon-type :pole)
			   +weapon-pole-range+
			   +weapon-melee-range+)))
    (when (and (mesh:can-use-movement-points-p attacker :minimum +attack-melee-cost+)
	       ;(game-state:entity-next-p (entity:state attacker) attacker defender)
	       (map-utils:facingp (mesh:calculate-cost-position attacker)
				  (entity:dir attacker)
				  (mesh:calculate-cost-position defender)
				  :max-distance max-dist-atk))
      (let ((msg (make-instance 'game-event:attack-melee-event
				:id-origin       (identificable:id attacker)
				:id-destination  (identificable:id defender)
				:attacker-entity attacker)))
	(mesh:decrement-move-points-attack-melee attacker)
	(mesh:set-attack-status attacker)
	(game-event:send-refresh-toolbar-event)
	(game-event:propagate-attack-melee-event msg)))))

(defun defend-from-attack-short-range (event)
  (let* ((attacker (game-event:attacker-entity event))
	 (defender (game-state:find-entity-by-id (entity:state attacker)
						 (game-event:id-destination event))))
    (assert (and attacker defender))
    (let* ((ghost-atk    (entity:ghost attacker))
	   (ghost-defend (entity:ghost defender))
	   (weapon       (character:worn-weapon ghost-atk))
	   (weapon-level (if weapon
			     (character:level weapon)
			     0.0))
	   (weapon-type  (character:weapon-type-short-range ghost-atk))
	   (attack-dmg        (character:actual-melee-attack-damage ghost-atk))
	   (bonus-attack-dmg  (cond
				((eq weapon-type :edge)
				 (character:actual-edge-weapons-damage-bonus ghost-atk))
				((eq weapon-type :impact)
				 (character:actual-impact-weapons-damage-bonus ghost-atk))
				((eq weapon-type :pole)
				 (character:actual-pole-weapons-damage-bonus ghost-atk))
				(t
				 0.0)))
	   (attack-chance        (character:actual-melee-attack-chance ghost-atk))
	   (bonus-attack-chance  (d+ (bonus-attack-weapon-combination
				      weapon-type
				      (character:weapon-type ghost-defend))
				     (bonus-defense-weapon-combination
				      weapon-type
				      (character:weapon-type ghost-defend))
				     (cond
				       ((eq weapon-type :edge)
					(character:actual-edge-weapons-chance-bonus ghost-atk))
				       ((eq weapon-type :impact)
					(character:actual-impact-weapons-chance-bonus ghost-atk))
				       ((eq weapon-type :pole)
					(character:actual-pole-weapons-chance-bonus ghost-atk))
				       (t
					0.0)))))
    (cond
      ((typep (entity:ghost defender) 'character:player-character)
       (let* ((armor-level  (cond
			     ((character:armorp (character:armor ghost-defend))
			      (character:level  (character:armor ghost-defend)))
			     (t
			      nil)))
	      (shield-level (cond
			      ((character:shieldp (character:left-hand ghost-atk))
			       (character:level (character:left-hand ghost-atk)))
			      ((character:shieldp (character:right-hand ghost-atk))
			       (character:level (character:right-hand ghost-atk)))
			      (t
			       nil)))
	      (dodge-chance         (character:actual-dodge-chance ghost-defend))
	      (ambushp              (and (vec~ (entity:dir attacker)
					       (entity:dir defender))
					 (pass-d100.0 (character:actual-ambush-attack-chance
						       ghost-atk)))))
	 (if weapon-type
	     (let ((dmg (attack-damage attack-dmg bonus-attack-dmg
				       attack-chance bonus-attack-chance
				       weapon-level
				       dodge-chance
				       shield-level
				       armor-level
				       ambushp)))
	       (if dmg
		   (progn
		     (send-effects-after-attack attacker defender)
		     (values dmg ambushp))
		   (values nil nil))))))
      ((typep (entity:ghost defender) 'character:np-character)
       (values (attack-damage attack-dmg bonus-attack-dmg
			      attack-chance bonus-attack-chance
			      weapon-level
			      0.0
			      0.0
			      (character:level ghost-defend)
			      nil)
	       nil))))))

(defun attack-long-range-animation (attacker defender)
  (when (renderp defender) ;; does it means: "is visible for someone?"
    (let* ((ghost-atk    (entity:ghost attacker))
	   (weapon-type  (character:weapon-type-long-range ghost-atk))
	   (cost         (cond
			   ((eq weapon-type :bow)
			    +attack-long-range-bow-cost+)
			   ((eq weapon-type :crossbow)
			    +attack-long-range-crossbow-cost+)
			   (t
			    0.0))))
      (if (and (> cost 0.0)
	       (mesh:can-use-movement-points-p attacker :minimum cost))
	  (progn
	    (mesh:decrement-move-points attacker cost)
	    (mesh:set-attack-status attacker)
	    t)
	  nil))))

(defun attack-spell-animation (attacker defender)
  (when (renderp defender) ;; does it means: "is visible for someone?"
    (let* ((ghost-atk    (entity:ghost attacker))
	   (spell        (character:spell-loaded ghost-atk))
	   (cost         (if spell
			     (spell:cost spell)
			     0.0)))
      (if (and (> cost 0.0)
	       (mesh:can-use-spell-points-p attacker :minimum cost))
	  (progn
	    (mesh:decrement-spell-points attacker cost)
	    (mesh:set-attack-spell-status attacker)
	    t)
	  nil))))

(defun spell-animation (attacker defender)
  (when (renderp defender) ;; does it means: "is visible for someone?"
    (let* ((ghost-atk    (entity:ghost attacker))
	   (spell        (character:spell-loaded ghost-atk))
	   (cost         (if spell
			     (spell:cost spell)
			     0.0)))
      (if (and (> cost 0.0)
	       (mesh:can-use-spell-points-p attacker :minimum cost))
	  (progn
	    (mesh:decrement-spell-points attacker cost)
	    (mesh:set-spell-status attacker)
	    t)
	  nil))))

(defun long-range-attack-cost (attacker)
  (let* ((ghost-atk    (entity:ghost attacker))
	 (weapon-type  (character:weapon-type-long-range ghost-atk))
	 (cost         (cond
			 ((eq weapon-type :bow)
			  +attack-long-range-bow-cost+)
			 ((eq weapon-type :crossbow)
			  +attack-long-range-crossbow-cost+)
			 (t
			  0.0))))
    cost))

(defun send-attack-long-range-event (attacker defender)
  (when (renderp defender) ;; does it means: "is visible for someone?"
    (let ((msg (make-instance 'game-event:attack-long-range-event
			      :id-origin       (identificable:id attacker)
			      :id-destination  (identificable:id defender)
			      :attacker-entity attacker)))
      (game-event:send-refresh-toolbar-event)
      (game-event:propagate-attack-long-range-event msg))))

(defun send-attack-spell-event (attacker defender)
  (when (renderp defender) ;; does it means: "is visible for someone?"
    (let* ((ghost-atk    (entity:ghost attacker))
	   (spell        (character:spell-loaded ghost-atk))
	   (msg (make-instance 'game-event:attack-spell-event
			       :id-origin       (identificable:id attacker)
			       :id-destination  (identificable:id defender)
			       :attacker-entity attacker
			       :spell           spell)))
      (game-event:send-refresh-toolbar-event)
      (game-event:propagate-attack-spell-event msg))))

(defun send-spell-event (attacker defender)
  (when (renderp defender) ;; does it means: "is visible for someone?"
    (let* ((ghost-atk    (entity:ghost attacker))
	   (spell        (character:spell-loaded ghost-atk))
	   (msg (make-instance 'game-event:spell-event
			       :id-origin       (identificable:id attacker)
			       :id-destination  (identificable:id defender)
			       :attacker-entity attacker
			       :spell           spell)))
      (game-event:send-refresh-toolbar-event)
      (game-event:propagate-spell-event msg))))

(defun height-variation-chance-fn (h-atk h-defend)
  (cond
    ((epsilon= h-atk h-defend)
     0.0)
    ((d> h-atk h-defend)
     (funcall (num:gaussian-function 20.0 5.0 20.0)
	      (d- h-atk h-defend)))
    (t
     -10.0)))

(defun actual-chance-long-range-attack (attacker defender)
    (assert (and attacker defender))
    (let* ((ghost-atk            (entity:ghost attacker))
	   (ghost-defend         (entity:ghost defender))
	   (weapon-type          (character:weapon-type-long-range ghost-atk))
	   (attack-chance        (character:actual-range-attack-chance ghost-atk))
	   (chance-decrement     (cond
				   ((eq weapon-type :crossbow)
				    +attack-long-range-crossbow-chance-decrement+)
				   ((eq weapon-type :bow)
				    +attack-long-range-bow-chance-decrement+)
				   (t
				    0.0)))
	   (dist                 (vec-length (vec- (entity:pos attacker)
						   (entity:pos defender))))
	   (chance-by-h          (height-variation-chance-fn (elt (entity:pos attacker) 1)
							     (elt (entity:pos defender) 1)))
	   (actual-attack-chance (max 0.0 (d+ (num:d* chance-decrement dist)
					      chance-by-h
					      attack-chance
					      (d+ (bonus-attack-weapon-combination
						   weapon-type
						   (character:weapon-type ghost-defend))
						  (bonus-defense-weapon-combination
						   weapon-type
						   (character:weapon-type ghost-defend)))))))
      (if weapon-type
	  actual-attack-chance
	  0.0)))

(defun defend-from-attack-long-range (event)
  (let* ((attacker (game-event:attacker-entity event))
	 (defender (game-state:find-entity-by-id (entity:state attacker)
						 (game-event:id-destination event))))
    (assert (and attacker defender))
    (let* ((ghost-atk    (entity:ghost attacker))
	   (ghost-defend (entity:ghost defender))
	   (weapon       (character:worn-weapon ghost-atk))
	   (weapon-type  (character:weapon-type-long-range ghost-atk))
	   (weapon-level (if weapon
			     (character:level weapon)
			     0.0))
	   (attack-dmg   (character:actual-range-attack-damage ghost-atk)))
      (cond
	((typep (entity:ghost defender) 'character:player-character)
	 (let* ((armor-level  (cond
				((character:armorp (character:armor ghost-defend))
				 (character:level  (character:armor ghost-defend)))
				(t
				 nil)))
		(shield-level (cond
				((character:shieldp (character:left-hand ghost-atk))
				 (character:level (character:left-hand ghost-atk)))
				((character:shieldp (character:right-hand ghost-atk))
				 (character:level (character:right-hand ghost-atk)))
				(t
				 nil)))
		(dodge-chance         (character:actual-dodge-chance ghost-defend))
		(ambushp              (and (vec~ (entity:dir attacker)
						 (entity:dir defender))
					   (pass-d100.0 (character:actual-ambush-attack-chance
							 ghost-atk))))
		(actual-attack-chance (actual-chance-long-range-attack attacker defender)))
	   (if weapon-type
	       (let ((dmg (attack-damage attack-dmg 0.0
					 actual-attack-chance 0.0
					 weapon-level
					 dodge-chance
					 shield-level
					 armor-level
					 ambushp)))
		 (if dmg
		   (progn
		     (send-effects-after-attack attacker defender)
		     (values dmg ambushp))
		   (values nil nil))))))
	((typep (entity:ghost defender) 'character:np-character)
	 (values (attack-damage attack-dmg  0.0
				(actual-chance-long-range-attack attacker defender) 0.0
				weapon-level
				0.0
				0.0
				(character:level ghost-defend)
				nil)
		 nil))))))

(defun defend-from-attack-spell (event)
 (let* ((attacker (game-event:attacker-entity event))
	(defender (game-state:find-entity-by-id (entity:state attacker)
						(game-event:id-destination event))))
    (assert (and attacker defender))
    (let* ((ghost-atk     (entity:ghost     attacker))
	   (ghost-defend  (entity:ghost     defender))
	   (spell         (game-event:spell event)))
      (%defend-from-attack-spell ghost-atk ghost-defend spell))))

(defun %defend-from-attack-spell (attacker defender spell)
  (let* ((attack-dmg (dmax 0.0
			   (gaussian-probability (d/ (spell:damage-inflicted spell) 2.0)
						 (d* (spell:damage-inflicted spell) 0.75)))))
    (cond
      ((typep defender 'character:player-character)
       (let* ((armor-level  (cond
			      ((character:armorp (character:armor defender))
			       (character:level  (character:armor defender)))
			      (t
			       nil)))
	      (shield-level (cond
			      ((character:shieldp (character:left-hand  attacker))
			       (character:level   (character:left-hand  attacker)))
			      ((character:shieldp (character:right-hand attacker))
			       (character:level   (character:right-hand attacker)))
			      (t
			       nil)))
	      (dodge-chance         (character:actual-dodge-chance defender))
	      (ambushp              (and (vec~ (entity:dir attacker)
					       (entity:dir defender))
					 (pass-d100.0 (character:actual-ambush-attack-chance
						       attacker)))))
	 (let ((dmg (attack-spell-damage attack-dmg
					 dodge-chance
					 shield-level
					 armor-level
					 ambushp)))
	   (if dmg
	       (progn
		 (send-effects-after-attack attacker
					    defender
					    :weapon spell)
		 (values dmg ambushp))
	       (values nil nil)))))
      ((typep defender 'character:np-character)
       (values (attack-spell-damage attack-dmg
				    0.0
				    nil
				    (character:level defender)
				    nil)
	       nil)))))

(defun defend-from-spell (event)
 (let* ((attacker (game-event:attacker-entity event))
	(defender (game-state:find-entity-by-id (entity:state attacker)
						(game-event:id-destination event))))
    (assert (and attacker defender))
    (let* ((spell         (game-event:spell event)))
      (cond
	((typep (entity:ghost defender) 'character:player-character)
	 (if (spell:use-custom-effects-p spell)
	     (funcall (character:basic-interaction-params spell) attacker defender)
	     (send-effects-after-attack attacker
					defender
					:weapon spell))
	 t)
	((typep (entity:ghost defender) 'character:np-character)
	 nil)))))

(defun attack-short-range (world attacker defender)
  "This is the most high level attack routine"
  (when defender
    (if (character:worn-weapon (entity:ghost attacker))
	(progn
	  (world:remove-all-tooltips world)
	  (battle-utils:send-attack-melee-event attacker defender))
	(make-tooltip-no-weapon-error world attacker))
    (entity:reset-tooltip-ct defender)
    (world:reset-toolbar-selected-action world)))

(defun attack-long-range (world attacker defender)
  (if (character:worn-weapon (entity:ghost attacker))
      (progn
	(world:remove-all-tooltips world)
	(when (attack-long-range-animation attacker defender)
	  (arrows:launch-arrow +default-arrow-name+
			       world
			       attacker
			       defender)))
      (make-tooltip-no-weapon-error world attacker))
  (entity:reset-tooltip-ct defender)
  (world:reset-toolbar-selected-action world)
  (game-event:send-refresh-toolbar-event))

(defun make-tooltip-no-weapon-error (world attacker)
  (world:post-entity-message world attacker
			     (format nil
				     (_"You have not got a weapon"))
			     nil))

(defun make-tooltip-no-spell-error (world attacker)
  (world:post-entity-message world attacker
			     (format nil
				     (_"You have not loaded a spell"))
			     nil))

(defun attack-launch-spell (world attacker defender)
  (when (and attacker
	     defender)
    (if (character:spell-loaded (entity:ghost attacker))
	(progn
	  (world:remove-all-tooltips world)
	  (when (attack-spell-animation attacker defender)
	    (arrows:launch-attack-spell (character:spell-loaded (entity:ghost attacker))
					world
					attacker
					defender)))
	(make-tooltip-no-spell-error world attacker))
    (entity:reset-tooltip-ct defender)
    (world:reset-toolbar-selected-action world)
    (game-event:send-refresh-toolbar-event)))

(defun launch-spell (world attacker defender)
  (when (and attacker
	     defender)
    (if (character:spell-loaded (entity:ghost attacker))
	(progn
	  (world:remove-all-tooltips world)
	  (when (spell-animation attacker defender)
	    (arrows:launch-spell (character:spell-loaded (entity:ghost attacker))
				 world
				 attacker
				 defender)))
	(make-tooltip-no-spell-error world attacker))
    (entity:reset-tooltip-ct defender)
    (world:reset-toolbar-selected-action world)
    (game-event:send-refresh-toolbar-event)))

(defun attack-statistics (weapon-level attack-dmg shield-level armor-level
			   &optional (count 10000))
  (macrolet ((fmt-comment (a fmt-params)
	       `(format t ,(text-utils:strcat "# " a) ,fmt-params)))
    (flet ((count-less-than (hits threshold)
	     (let ((less (count-if #'(lambda (a) (< a (* attack-dmg threshold))) hits)))
	       (* 100.0 (/ less (length hits))))))
      (fmt-comment "weapon level: ~a~%" weapon-level)
      (fmt-comment "max damage  : ~a~%" attack-dmg)
      (fmt-comment "shield-level: ~a~%" shield-level)
      (fmt-comment "armor-level : ~a~%" armor-level)
      (let ((all-damages (loop repeat count collect
			      (attack-damage attack-dmg
					     0.0
					     100.0 0.0
					     weapon-level
					     0.0
					     shield-level
					     armor-level))))
	(fmt-comment "Hit with damage above than 80%: ~,2@f~%"
		     (- 100 (count-less-than all-damages 0.8)))
	(fmt-comment "Hit with damage below than 80%: ~,2@f~%"
		     (count-less-than all-damages 0.8))
	(fmt-comment "Hit with damage below than 50%: ~,2@f~%"
		     (count-less-than all-damages 0.5))
	(fmt-comment "Hit with damage below than 20%: ~,2@f~%"
		     (count-less-than all-damages 0.2))))))

(defun attack-spell-vs-inert-statistics (spell-id &optional (count 1000))
  (loop for level from 1 to 9 do
       (format t "; level ~a~%"  level)
       (let* ((data (loop repeat count collect
			 (%defend-from-attack-spell nil
						    (random-inert-object:generate-inert-object
						     (d level))
						    (spell:get-spell spell-id))))
	      (max  (reduce #'(lambda (a b) (if (< a b) b a)) data :initial-value -1.0))
	      (min  (reduce #'(lambda (a b) (if (> a b) b a)) data :initial-value 1e10)))
	 (format t "; minimum damage ~a~%" min)
	 (format t "; maximum damage ~a~%" max)
	 (format t "; average damage ~a~%" (/ (reduce #'+ data) (length data)))
	 (format t "; median         ~a~2%" (median data)))))
