;; dawn of the Era: a tactical game.
;; Copyright (C) 2018  cage

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

(in-package :strategic-ai)

(define-constant +very-low+                   :very-low         :test #'eq)

(define-constant +low+                        :low              :test #'eq)

(define-constant +medium+                     :medium           :test #'eq)

(define-constant +high+                       :high             :test #'eq)

(define-constant +very-high+                  :very-high        :test #'eq)

(define-constant +my-faction+                 :my-faction       :test #'eq)

(define-constant +opponent-faction+           :opponent-faction :test #'eq)

(define-constant +size-vulnerability-concern+ 2                 :test #'eq)

(define-constant +min-facts-to-build-tree+    10                :test #'=)

(defparameter    *decision-tree*              nil)

(defun faction-map-under-control-0to1 (influence-map faction)
  (let* ((area        (* (width  influence-map)
                         (height influence-map)))
         (ai-area     (reduce #'+ (remove-if #'(lambda (a)
                                                 (epsilon= (sign a) +influence-human-sign+))
                                             (data influence-map))))
         (ai-fraction (/ ai-area area)))
    (if (eq faction +pc-type+)
        (d (- 1.0 ai-fraction))
        (d ai-fraction))))

(defun alive-factions-components (blackboard faction)
  (with-accessors ((main-state main-state)) blackboard
    (if (eq faction +npc-type+)
        (map-ai-entities     main-state #'identity) ; remove dead
        (map-player-entities main-state #'identity))))

(defgeneric make-influence-map (object &key matrix))

(defgeneric faction-map-under-control (object faction))

(defgeneric faction-influence-map (object faction))

(defgeneric faction-vulnerability-map (object faction))

(defgeneric faction-tension-map (object faction))

(defmethod make-influence-map ((object game-state) &key (matrix nil))
  (with-accessors ((player-entities player-entities)
                   (map-state map-state)
                   (ai-entities ai-entities)) object
    (make-influence-map* object ai-entities player-entities :matrix matrix)))

(defun make-influence-map* (game-state ai-entities player-entities &key (matrix nil))
  (flet ((combining (a b) (d+ a b)))
    (with-accessors ((map-state map-state)) game-state
      (let* ((im            (or matrix
                                (make-matrix (width map-state) (height map-state) 0.0)))
             (influence-pc  #'(lambda (influencer)
                                (d* +influence-human-sign+
                                    (character:calculate-influence (ghost influencer)))))
             (influence-npc #'(lambda (influencer)
                                (d* +influence-ai-sign+
                                    (character:calculate-influence (ghost influencer))))))
        (inmap:apply-influence im player-entities #'combining influence-pc)
        (inmap:apply-influence im ai-entities     #'combining influence-npc)
        im))))

(defun make-tension-map* (game-state ai-entities player-entities &key (matrix nil))
  (flet ((combining (a b) (d+ a b)))
    (with-accessors ((map-state map-state)) game-state
      (let* ((im            (or matrix
                                (make-matrix (width map-state) (height map-state) 0.0)))
             (influence-fn  #'(lambda (influencer)
                                (character:calculate-influence (ghost influencer)))))
        (inmap:apply-influence im player-entities #'combining influence-fn)
        (inmap:apply-influence im ai-entities     #'combining influence-fn)
        im))))


(defmethod faction-influence-map ((object blackboard) faction)
  (with-accessors ((main-state         main-state)) object
    (let* ((id->entity-fn             #'(lambda (a) (find-entity-by-id main-state a)))
           (visible-from-me-fn        (if (eq faction +npc-type+)
                                          #'all-player-id-visible-from-ai
                                          #'all-ai-id-visible-from-player))
           (ai-entities               (if (eq faction +npc-type+)
                                          (alive-factions-components object faction)
                                          (map 'list
                                               id->entity-fn
                                               (funcall visible-from-me-fn main-state))))
           (pc-entities               (if (eq faction +npc-type+)
                                          (map 'list
                                               id->entity-fn
                                               (funcall visible-from-me-fn main-state))
                                          (alive-factions-components object +pc-type+))))
      (make-influence-map* main-state
                           ai-entities
                           pc-entities
                           :matrix nil))))

;; TODO refactor away duplicated code, see faction-influence-map
(defmethod faction-vulnerability-map ((object blackboard) faction)
  (flet ((combining (a b) (d+ a b)))
    (macrolet ((with-power ((faction movement-weight-pc movement-weight-npc) &body body)
                 `(let ((*power-weights* (if (eq ,faction +pc-type+)
                                             (make-power-weights :movement ,movement-weight-pc)
                                             (make-power-weights :movement ,movement-weight-npc))))
                    ,@body)))
      (with-accessors ((main-state main-state)) object
        (with-accessors ((map-state map-state)) main-state
          (let* ((id->entity-fn      #'(lambda (a) (find-entity-by-id main-state a)))
                 (visible-from-me-fn (if (eq faction +npc-type+)
                                         #'all-player-id-visible-from-ai
                                         #'all-ai-id-visible-from-player))
                 (ai-entities        (if (eq faction +npc-type+)
                                         (alive-factions-components object faction)
                                         (map 'list
                                              id->entity-fn
                                              (funcall visible-from-me-fn main-state))))
                 (pc-entities        (if (eq faction +npc-type+)
                                         (map 'list
                                              id->entity-fn
                                              (funcall visible-from-me-fn main-state))
                                         (alive-factions-components object +pc-type+)))
                 (im                 (make-matrix (width map-state) (height map-state) 0.0))
                 (influence-pc       #'(lambda (influencer)
                                         (with-power (faction 0.0 2.0)
                                           (d* +influence-human-sign+
                                               (calculate-influence (ghost influencer))))))
                 (influence-npc      #'(lambda (influencer)
                                         (with-power (faction 2.0 0.0)
                                           (d* +influence-ai-sign+
                                               (calculate-influence (ghost influencer)))))))
          (inmap:apply-influence im pc-entities #'combining influence-pc)
          (inmap:apply-influence im ai-entities #'combining influence-npc)
          im))))))

(defmethod faction-tension-map ((object blackboard) faction)
  (with-accessors ((main-state         main-state)) object
    (let* ((id->entity-fn             #'(lambda (a) (find-entity-by-id main-state a)))
           (visible-from-me-fn        (if (eq faction +npc-type+)
                                          #'all-player-id-visible-from-ai
                                          #'all-ai-id-visible-from-player))
           (ai-entities               (if (eq faction +npc-type+)
                                          (alive-factions-components object faction)
                                          (map 'list
                                               id->entity-fn
                                               (funcall visible-from-me-fn main-state))))
           (pc-entities               (if (eq faction +npc-type+)
                                          (map 'list
                                               id->entity-fn
                                               (funcall visible-from-me-fn main-state))
                                          (alive-factions-components object +pc-type+))))
      (make-tension-map* main-state
                         ai-entities
                         pc-entities
                         :matrix nil))))

(defmethod faction-map-under-control ((object matrix) faction)
  (let ((sum (reduce #'+ (data object))))
    (if (epsilon= +influence-ai-sign+ (sign sum))
        (if (eq faction +npc-type+) ;; ai rules
            +my-faction+
            +opponent-faction+)
        (if (eq faction +npc-type+) ;; human rules
            +opponent-faction+
            +my-faction+))))

(defmethod faction-map-under-control ((object blackboard) faction)
  (faction-map-under-control (faction-influence-map object faction) faction))

(defun dmg-points-ratio (blackboard faction)
  (flet ((reduce-dmg (entities fn)
           (reduce #'+
                   (map 'list #'(lambda (a) (funcall fn (ghost a))) entities)
                   :initial-value 0)))
    (let* ((entities (alive-factions-components blackboard faction))
           (all-current-dmg  (reduce-dmg entities  #'current-damage-points))
           (all-max-dmg      (reduce-dmg entities  #'actual-damage-points)))
      (if entities
          (/ all-current-dmg all-max-dmg)
          0.0))))

(defun average-dist-entities (blackboard entities)
  (flet ((calc-dist (a b)
           (let* ((pos-a (calculate-cost-position a))
                  (pos-b (calculate-cost-position b)))
             (multiple-value-bind (path total-cost costs)
                 (blackboard:path-w/o-concerning-tiles blackboard
                                                       pos-a
                                                       pos-b
                                                       :cut-off-first-tile  nil
                                                       :allow-path-length-1 t)
               (declare (ignore path costs))
                 total-cost))))
  (let ((all-pairs (ordered-pairs-no-twins entities entities)))
    (if all-pairs
        (d (/ (reduce #'+
                      (map 'list #'(lambda (a) (calc-dist (car a) (cdr a))) all-pairs)
                      :initial-value 0)
              (length all-pairs)))
        0.0))))

(defun average-cost-faction (blackboard faction)
  (with-accessors ((main-state main-state)) blackboard
    (if (eq faction +npc-type+)
        (average-dist-entities blackboard (ai-entities     main-state))
        (average-dist-entities blackboard (player-entities main-state)))))

(defun %average-dmg-wizards (entities)
  (let ((all-wizards (remove-if-not #'(lambda (a) (character:pclass-wizard-p (ghost a)))
                                    entities)))
    (if all-wizards
        (d (/ (reduce #'+
                      (map 'list #'(lambda (a) (current-damage-points (ghost a))) all-wizards)
                      :initial-value 0)
              (length all-wizards)))
        0.0)))

(defun average-dmg-wizards (blackboard faction)
  (with-accessors ((main-state main-state)) blackboard
    (let ((all-wizards (if (eq faction +npc-type+)
                           (ai-entities     main-state)
                           (player-entities main-state))))
      (%average-dmg-wizards all-wizards))))

(defun entities-vulnerables (blackboard faction)
  (with-accessors ((main-state main-state)) blackboard
    (flet ((in-danger-p (pos vulnerability-map)
             (find-if #'(lambda (v)
                          (2d-utils:displace-2d-vector (v x y)
                            (if (eq faction +pc-type+)
                                (epsilon= (sign (matrix-elt vulnerability-map y x))
                                          +influence-ai-sign+)
                                (epsilon= (sign (matrix-elt vulnerability-map y x))
                                          +influence-human-sign+))))
                      pos)))
      (let ((vulnerability-map (faction-vulnerability-map blackboard faction))
            (loop-fn           (if (eq faction +pc-type+)
                                   #'map-player-entities
                                   #'map-ai-entities)))
        (remove-if-null
         (funcall loop-fn
                  main-state
                  #'(lambda (entity)
                      (let* ((pos  (calculate-cost-position entity))
                             (near (gen-valid-neighbour-position-in-box vulnerability-map
                                                                        (ivec2-x pos)
                                                                        (ivec2-y pos)
                                                                        +size-vulnerability-concern+
                                                                        +size-vulnerability-concern+
                                                                        :add-center nil)))
                        (if (in-danger-p near vulnerability-map)
                            entity
                            nil)))))))))

(defun visible-opponents (blackboard faction)
  (faction-all-visibles-opponents blackboard faction :alive-only t))

(defun visible-friends (blackboard faction)
  (faction-all-visibles-opponents blackboard
                                  (faction->opposite-faction faction)
                                  :alive-only t))

(defun dump-ai-facts (data)
  (let ((dump-file (res:get-resource-file +decision-tree-facts-file+
                                          +decision-tree-data-resource+
                                          :if-does-not-exists :create)))
    (with-open-file (stream dump-file :direction :output :if-exists :supersede)
      (format stream "~s~%" data))))

(defun register-ai-tree-data (world)
  (flet ((init-data (file)
           (with-open-file (s file :direction :io :if-exists :overwrite)
             (when s
               (let ((l (file-length s)))
                 (when (or (null l)
                           (= l 0))
                   (format s "~s" (list +ai-fact-header+))))))
           (with-open-file (stream file)
             (read stream))))
    (when (and (faction-turn-human-p world)
               (gconf:config-train-ai))
      (with-accessors ((main-state main-state)) world
        (with-accessors ((blackboard blackboard)) main-state
          (let* ((influence-map     (strategic-ai:faction-influence-map blackboard
                                                                        +pc-type+))
                 (map-under-control (strategic-ai:faction-map-under-control influence-map
                                                                            +pc-type+))
                 (dmg-ratio         (strategic-ai:dmg-points-ratio   blackboard +pc-type+))
                 (average-dist      (strategic-ai:average-cost-faction blackboard +pc-type+))
                 (vulnerables-units (length (strategic-ai:entities-vulnerables blackboard
                                                                               +pc-type+)))
                 (visible-opponents (length (strategic-ai:visible-opponents    blackboard
                                                                               +npc-type+)))
                 (visible-pc        (length (strategic-ai:visible-opponents    blackboard
                                                                               +pc-type+)))
                 (wizard-dmg        (strategic-ai:average-dmg-wizards          blackboard
                                                                               +pc-type+))
                 (new-row           (list map-under-control
                                          dmg-ratio
                                          average-dist
                                          vulnerables-units
                                          visible-opponents
                                          visible-pc
                                          wizard-dmg))
                 (dump-file         (res:get-resource-file +decision-tree-facts-file+
                                                           +decision-tree-data-resource+
                                                           :if-does-not-exists :create))
                 (data              (init-data dump-file)))
            (setf data (append data (list new-row)))
            (if (dump-ai-facts data)
                data
                nil)))))))

(defun register-strategy-from-human (world)
  (when (and (faction-turn-human-p world)
             (gconf:config-train-ai))
    (mtree:add-child (world:gui world)
                     (train-ai-window:make-train-ai-window (compiled-shaders world)))))

(defun fact-file ()
  (res:get-resource-file +decision-tree-facts-file+
                         +decision-tree-data-resource+
                         :if-does-not-exists nil))

(defun read-facts-file ()
  (when-let* ((fact-file (fact-file))
              (facts     (fs:read-single-form fact-file)))
    facts))

;; TODO test needed, probably broken
(defun build-decision-tree (&key (set-parameter t))
  (when-let* ((fact-file (fact-file))
              (facts     (read-facts-file))
              (tree-file (res:get-resource-file +decision-tree-file+
                                                +decision-tree-data-resource+
                                                :if-does-not-exists :create)))
    (when (and (>= (length facts)
                   +min-facts-to-build-tree+)
               (fs:file-outdated-p tree-file fact-file))
      (let ((decision-tree (id3:build-tree (rest facts) ; remove header
                                           +ai-fact-header+ :prune t)))
        (with-open-file (stream tree-file
                                :direction         :output
                                :if-does-not-exist :create
                                :if-exists         :supersede)
          (format stream "~s" (serialize decision-tree))
          #+(and debug-mode debug-ai)
          (misc:dbg "decision tree~%~a~%-> ~a" decision-tree
                    (fs:file-outdated-p tree-file fact-file))
          (when set-parameter
            (setf *decision-tree* decision-tree))
          decision-tree)))))
