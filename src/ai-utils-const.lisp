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

(in-package :ai-utils)

(define-constant +pole-key-tactics+                :pole                       :test #'eq)

(define-constant +melee-key-tactics+               :melee                      :test #'eq)

(define-constant +bow-key-tactics+                 :bow                        :test #'eq)

(define-constant +crossbow-key-tactics+            :crossbow                   :test #'eq)

(define-constant +planner-file-extension+          ".lisp"
  :test #'string=)

(define-constant +sink-action+                     :sink                       :test #'eq
 :documentation "This is the default action of the planner for blacklisting")

(define-constant +plan-stopper+                    :end                        :test #'eq)

(define-constant +idle-action+                     :idle                       :test #'eq)

(define-constant +interrupt-action+                :interrupt                  :test #'eq)

(define-constant +rotate-action+                   :rotate                     :test #'eq)

(define-constant +move-action+                     :move                       :test #'eq)

(define-constant +faint-action+                    :faint                      :test #'eq)

(define-constant +go-to-attack-pos-action+         :go-to-attack-pos           :test #'eq)

(define-constant +interrupted-action+              :interrupt                  :test #'eq)

(define-constant +launch-heal-spell-action+        :launch-heal-spell          :test #'eq)

(define-constant +launch-heal-spell-friend-action+ :launch-heal-spell-friend   :test #'eq)

(define-constant +launch-teleport-spell-action+    :launch-teleport-spell      :test #'eq)

(define-constant +launch-wall-break-spell-action+  :launch-spell-wall          :test #'eq)

(define-constant +hide-action+                     :hide                       :test #'eq)

(define-constant +flee-action+                     :flee                       :test #'eq)

(define-constant +find-hiding-place-action+        :find-hiding-place          :test #'eq)

(define-constant +place-trap-action+               :place-trap                 :test #'eq)

(define-constant +attack-action+                   :attack                     :test #'eq)

(define-constant +go-to-attack-pos-action+         :go-to-attack-pos           :test #'eq)

(define-constant +find-attack-pos-action+          :find-attack-position       :test #'eq)

(define-constant +load-weapon-action+              :load-weapon                :test #'eq)

(define-constant +go-near-to-attack-pos-action+    :go-near-to-attack-pos      :test #'eq)

(define-constant +go-near-weak-friend-atk-action+  :go-near-weak-friend-attack :test #'eq)

(define-constant +go-near-weak-friend-action+      :go-near-weak-friend        :test #'eq)

(define-constant +protect-attack-action+           :protect-attack             :test #'eq)

(define-constant +protect-attack-spell-action+     :protect-attack-spell       :test #'eq)

(define-constant +protect-action+                  :protect                    :test #'eq)

(define-constant +attack-spell-action+             :launch-attack-spell        :test #'eq)

(define-constant +find-fountain-action+            :find-fountain              :test #'eq)

(define-constant +go-to-fountain-action+           :go-to-fountain             :test #'eq)

(define-constant +go-to-attack-spell-pos-action+   :go-to-attack-spell-pos     :test #'eq)

(define-constant +use-fountain+                    :use-fountain               :test #'eq)

(define-constant +min-chain-teleport+              3                           :test #'=)
