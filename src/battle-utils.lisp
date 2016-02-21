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

(define-constant +attack-max-sigma+               80.0  :test #'=)

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
	      (d* 2.8 (d- shield-level 1.0))
	      0.0))
      (d- (if armour-level
	      (d* 2.8 (d- armour-level 1.0))
	      0.0))))

(defun attack-gaussian-fn (attack-dmg bonus-attack-dmg weapon-level
			   &optional (shield-level nil) (armor-level nil))
  (let ((sigma      (calc-attack-sigma weapon-level attack-dmg shield-level armor-level))
	(max-damage (d+ attack-dmg bonus-attack-dmg)))
    (standard-battle-gaussian-fn max-damage sigma)))

(defun attack (attack-dmg    bonus-attack-dmg
	       attack-chance bonus-attack-chance
	       weapon-level
	       dodge-chance
	       &optional (shield-level nil) (armor-level nil))
  (if (pass-d100.0 (max (d- (d+ attack-chance bonus-attack-chance)
			    dodge-chance)
			0.0))
      (let ((gaussian-fn (attack-gaussian-fn attack-dmg
					     bonus-attack-dmg
					     weapon-level
					     shield-level
					     armor-level)))
	(funcall gaussian-fn
		 (d- 100.0 (lcg-next-upto 100.0))   ; attacker
		 (d- 100.0 (lcg-next-upto 100.0)))) ; defender
      nil))

(defun dump-gaussian (producer-fn)
  (loop for x from -0.0 below 100.0 by 1.0 do
       (loop for y from -0.0 below 100.0 by 1.0 do
	    (format t "~a ~a ~a~%" x y (funcall producer-fn x y)))))

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
			      (attack attack-dmg
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
