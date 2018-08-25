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

(in-package :level-config)

(alexandria:define-constant +particle-name-prefix+ "particle-" :test #'string=)

(defparameter *particle-names* 0)

(defun gen-particle-name ()
  (prog1
      (format nil "~a~a.tga" +particle-name-prefix+ *particle-names*)
    (incf *particle-names*)))

(alexandria:define-constant +error-wrong-type+ "Expected a ~s got ~s (a ~a) instead"
  :test #'string=)

(alexandria:define-constant +error-wrong-keyword+ "Expected ~s got ~s instead"
  :test #'string=)

(alexandria:define-constant +error-cannot-create-texture-from-function+
    "Error can not create texture from function ~a" :test #'string=)

(alexandria:define-constant +error-cannot-create-texture-from-string+
    "Error can not create texture from string ~a" :test #'string=)

(alexandria:define-constant +error-dependency-not-satisfied+
    "Error: dependency not satisfied, ~a depends on ~a" :test #'string=)

(alexandria:define-constant +color-prefix+      "c" :test #'string=)

(alexandria:define-constant +offset-function+     14 :test #'=)

(alexandria:define-constant +offset-file+         11 :test #'=)

(alexandria:define-constant +offset-string+       11 :test #'=)

(alexandria:define-constant +offset-postprocess+   4 :test #'=)

(alexandria:define-constant +map-cache-name+   "map" :test #'string=)

(alexandria:define-constant +available-level-wall+ (list texture:+texture-tag-wall-level-1+
                                                         texture:+texture-tag-wall-level-2+
                                                         texture:+texture-tag-wall-level-3+)
  :test #'equalp)

(alexandria:define-constant +available-level-floor+ (list texture:+texture-tag-floor-level-1+
                                                          texture:+texture-tag-floor-level-2+
                                                          texture:+texture-tag-floor-level-3+)
  :test #'equalp)

(alexandria:define-constant +available-level-door+ (list texture:+texture-tag-door-level-1+
                                                          texture:+texture-tag-door-level-2+
                                                          texture:+texture-tag-door-level-3+)
  :test #'equalp)

(alexandria:define-constant +available-level-ceil+ (list texture:+texture-tag-ceil-level-1+
                                                          texture:+texture-tag-ceil-level-2+
                                                          texture:+texture-tag-ceil-level-3+)
  :test #'equalp)

(alexandria:define-constant +saved-seed-key+ "random-seed" :test #'string=)

(alexandria:define-constant +max-progress-slot+    0.4  :test #'=)

(alexandria:define-constant +inc-progress-slot+    0.01 :test #'=)

(defmacro err (str &rest param)
  `(format nil ,str ,@param))

(defmacro need-type ((what needed) &body body)
  `(if (typep ,what ,needed)
       (progn ,@body)
       (err +error-wrong-type+ ,needed ,what (type-of ,what))))

(defmacro need-keyword ((what needed) &body body)
  `(if (eq (alexandria:make-keyword ,what) ,needed)
       (progn ,@body)
       (err +error-wrong-keyword+ ,needed ,what)))

(defmacro need-keyword-if ((what needed) if-true if-false)
  `(if (eq (alexandria:make-keyword ,what) ,needed)
       ,if-true
       ,if-false))

(defun look-ahead-eq (body offset key)
  (and (< offset (length body))
       (eq key (alexandria:make-keyword (elt body offset)))))

(defmacro set-key-value (output var type)
  `(need-type (,var ,type)
    (setf ,output ,var)))

(defun set-value-random (bag)
  (cond
    ((or (every #'floatp   bag)
         (every #'integerp bag))
     (assert (> (length bag) 1))
     (num:lcg-next-in-range (first bag) (second bag)))
    ((or (every #'listp bag)
         (every #'arrayp bag))
     (elt bag (num:lcg-next-upto (length bag))))
    (t
     (alexandria:make-keyword (elt bag (num:lcg-next-upto (length bag)))))))

(defun process-parameter-list (params)
  (do ((l params)
       (output '()))
      ((not l) (reverse output))
    (if (numberp (elt l 0))
        (progn
          (push (elt l 0) output)
          (setf l (rest l)))
        (case (alexandria:make-keyword (elt l 0))
          (:random
           (push (set-value-random (elt l 1)) output)
           (setf l (subseq l 2)))
          (otherwise
           (push (alexandria:make-keyword (elt l 0)) output)
           (setf l (rest l)))))))

(defparameter *wants-complete-parsing* t)

(defparameter *main-window* nil)

(defparameter *renderer*    nil)

(defparameter *progress*    0.0)

(defparameter *level-name*        nil)

(defparameter *level-name-color*  nil)

(defparameter *level-notes*       nil)

(defparameter *raw-seed* "")

(defparameter *seed-after-loading* "")

(defparameter *game-hour* 0)

(defparameter *map* nil)

(defparameter *clouds* :dummy)

(defparameter *building-level* 0)

(defparameter *trees*                      '())

(defparameter *wall*                       nil)

(defparameter *window*                     nil)

(defparameter *door-n*                     nil)

(defparameter *door-s*                     nil)

(defparameter *door-e*                     nil)

(defparameter *door-w*                     nil)

(defparameter *chair-n*                    nil)

(defparameter *chair-s*                    nil)

(defparameter *chair-e*                    nil)

(defparameter *chair-w*                    nil)

(defparameter *floor*                      nil)

(defparameter *furnitures*                 '())

(defparameter *containers-furnitures*      '())

(defparameter *magic-furnitures*           '())

(defparameter *pillar-furnitures*          '())

(defparameter *chair-furnitures*           '())

(defparameter *table-furnitures*           '())

(defparameter *walkable-furnitures*        '())

(defparameter *wall-decoration-furnitures* '())

(defparameter *trap*                   nil)

(defun limited-progress ()
  (setf *progress* (min +max-progress-slot+ (d+ *progress* +inc-progress-slot+)))
  (update-progress *progress*))

(defun update-progress (progress)
  (setf (widget:progress (elt (mtree:children (world:gui *renderer*)) 0))
        progress)
  (sdl2.kit:render *main-window*))

(defun gen-normalmap-if-needed (diffuse-texture)
  (let ((roughness (texture:n-roughness diffuse-texture)))
    (if (not (epsilon= roughness 0.0))
        (texture:gen-name-and-inject-in-database (pixmap:gen-normal-map diffuse-texture
                                                                        :roughness roughness))
        nil)))

(defun setup-walls ()
  (let* ((tag        (elt +available-level-wall+ *building-level*))
         (texture    (random-elt (texture:list-of-texture-by-tag tag)))
         (normal-map (gen-normalmap-if-needed texture))
         (mesh       (mesh:parallelepiped +wall-w+
                                          +wall-h+
                                          :start-s-texture  0.0
                                          :max-s-texture    4.0
                                          :start-t-texture  0.0
                                          :end-t-texture    +wall-h-scale+
                                          :manifold         nil
                                          :compact-vertices nil
                                          :average-normals  nil
                                          :draw-top         t
                                          :draw-bottom      nil)))
    (mesh:gen-tangents mesh)
    (setf (texture:use-mipmap texture) t)
    (setf (texture:interpolation-type texture) :linear)
        (interfaces:prepare-for-rendering texture)
    (when normal-map
      (setf (texture:use-mipmap normal-map) t)
      (setf (texture:interpolation-type normal-map) :linear)
      (setf (texture:tags normal-map) #("normalmap wall"))
      (interfaces:prepare-for-rendering normal-map))
    (setf (mesh:texture-object mesh) texture)
    (mesh:get-material-from-texture mesh)
    (setf (mesh:normal-map mesh)     normal-map)
    (interfaces:prepare-for-rendering mesh)
    (setf *wall* mesh)))

(defun setup-window ()
  (let* ((tag        (elt +available-level-wall+ *building-level*))
         (texture    (random-elt (texture:list-of-texture-by-tag tag)))
         (normal-map (gen-normalmap-if-needed texture))
         (mesh       (mesh:parallelepiped +wall-w+
                                          (d* 0.6 +wall-h+)
                                          :start-s-texture  0.0
                                          :max-s-texture    4.0
                                          :start-t-texture  0.0
                                          :end-t-texture    +wall-h-scale+
                                          :manifold         nil
                                          :compact-vertices nil
                                          :average-normals  nil
                                          :draw-top         t
                                          :draw-bottom      nil)))
    (mesh:gen-tangents mesh)
    (setf (texture:use-mipmap texture) t)
    (setf (texture:interpolation-type texture) :linear)
        (interfaces:prepare-for-rendering texture)
    (when normal-map
      (setf (texture:use-mipmap normal-map) t)
      (setf (texture:interpolation-type normal-map) :linear)
      (setf (texture:tags normal-map) #("normalmap door"))
      (interfaces:prepare-for-rendering normal-map))
    (setf (mesh:texture-object mesh) texture)
    (mesh:get-material-from-texture mesh)
    (setf (mesh:normal-map mesh)     normal-map)
    (interfaces:prepare-for-rendering mesh)
    (setf *window* mesh)))

(defun setup-doors ()
  (let* ((tag        (elt +available-level-door+ *building-level*))
         (texture    (random-elt (texture:list-of-texture-by-tag tag)))
         (normal-map (gen-normalmap-if-needed texture))
         (mesh (mesh:parallelepiped +wall-w+
                                    +wall-h+
                                    :start-s-texture  0.0
                                    :max-s-texture    4.0
                                    :start-t-texture  0.0
                                    :end-t-texture    2.0
                                    :manifold         nil
                                    :compact-vertices nil
                                    :average-normals  nil
                                    :draw-top         t
                                    :draw-bottom      nil)))
    (mesh:gen-tangents mesh)
    (setf (texture:use-mipmap texture)    t)
    (setf (texture:interpolation-type texture) :linear)
        (interfaces:prepare-for-rendering texture)
    (when normal-map
      (setf (texture:use-mipmap normal-map) t)
      (setf (texture:interpolation-type normal-map) :linear)
      (setf (texture:tags normal-map) #("normalmap door"))
      (interfaces:prepare-for-rendering normal-map))
    (setf (mesh:texture-object mesh) texture)
    (mesh:get-material-from-texture mesh)
    (setf (mesh:normal-map mesh)     normal-map)
    (mesh:transform-vertices mesh (sb-cga:scale* 1.0 1.0 0.25))
    (interfaces:prepare-for-rendering mesh)
    (setf *door-n* (mesh:prepare-for-rendering (interfaces:clone mesh))
          *door-s* (mesh:prepare-for-rendering (interfaces:clone mesh))
          *door-e* (mesh:prepare-for-rendering (mesh:transform-vertices
                                                (interfaces:clone mesh)
                                                (sb-cga:rotate-around +y-axe+ +pi/2+)))
          *door-w* (mesh:prepare-for-rendering (mesh:transform-vertices
                                                (interfaces:clone mesh)
                                                (sb-cga:rotate-around +y-axe+ +pi/2+))))))

(defun setup-chairs ()
  (let* ((mesh (misc:random-elt *chair-furnitures*)))
    (setf *chair-s* (mesh:prepare-for-rendering (interfaces:clone mesh))
          *chair-e* (mesh:prepare-for-rendering (mesh:transform-vertices
                                                 (interfaces:clone mesh)
                                                 (sb-cga:rotate-around +y-axe+ (- +pi/2+))))
          *chair-n* (mesh:prepare-for-rendering (mesh:transform-vertices
                                                 (interfaces:clone mesh)
                                                 (sb-cga:rotate-around +y-axe+ +pi+)))
          *chair-w* (mesh:prepare-for-rendering (mesh:transform-vertices
                                                (interfaces:clone mesh)
                                                (sb-cga:rotate-around +y-axe+ +pi/2+))))))

(defun setup-floor ()
   (let* ((tag        (elt +available-level-floor+ *building-level*))
          (texture    (random-elt (texture:list-of-texture-by-tag tag)))
          (normal-map (gen-normalmap-if-needed texture))
          (mesh       (building-floor-mesh:floor-tile +terrain-chunk-tile-size+
                                                      +terrain-chunk-tile-size+)))
     (mesh:gen-tangents mesh)
     (setf (texture:use-mipmap texture)    t)
     (setf (texture:interpolation-type texture) :linear)
     (interfaces:prepare-for-rendering texture)
     (when normal-map
       (setf (texture:use-mipmap normal-map) t)
       (setf (texture:interpolation-type normal-map) :linear)
       (setf (texture:tags normal-map) #("normalmap floor"))
       (interfaces:prepare-for-rendering normal-map))
     (setf (mesh:texture-object mesh) texture)
     (mesh:get-material-from-texture mesh)
     (setf (mesh:normal-map mesh)     normal-map)
     (interfaces:prepare-for-rendering mesh)
     (setf *floor* mesh)))

(defun clean-global-vars ()
  (setf *level-name*                 nil
        *level-name-color*           nil
        *level-notes*                nil
        *particle-names*             0
        *progress*                   0.0
        *main-window*                nil
        *renderer*                   nil
        *map*                        nil
        *trees*                      '()
        *furnitures*                 '()
        *containers-furnitures*      '()
        *magic-furnitures*           '()
        *pillar-furnitures*          '()
        *chair-furnitures*           '()
        *table-furnitures*           '()
        *walkable-furnitures*        '()
        *wall-decoration-furnitures* '()
        *wall*                       nil
        *window*                     nil
        *door-n*                     nil
        *door-s*                     nil
        *door-e*                     nil
        *door-w*                     nil
        *chair-n*                    nil
        *chair-s*                    nil
        *chair-e*                    nil
        *chair-w*                    nil
        *floor*                      nil
        *trap*                  nil))

(defmacro define-level (&body body)
  (ensure-cache-running
    (p-name body)
    (setf body (subseq body 4))
    (p-notes body)
    (setf body (subseq body 3))
    (when *wants-complete-parsing*
      (need-keyword ((first body) :set)
        (need-keyword ((second body) :seed)
          (need-type ((third body) 'string)
            (setf num:*lcg-seed* (num:string-fnv-hash-32 (third body)))
            (setf *game-hour* (num:lcg-next-upto 24)
                  *raw-seed*  (third body))
            #+debug-mode (misc:dbg "hour ~a" *game-hour*)
            (branch-generate (subseq body 3)))
          (setup-floor)
          (limited-progress)
          (setup-walls)
          (limited-progress)
          (setup-window)
          (limited-progress)
          (setup-doors)
          (limited-progress)
          (setup-seed)
          (setup-chairs)
          (limited-progress))))))

(defun p-name (body)
  (need-keyword ((first body) :set)
    (need-keyword ((second body) :name)
      (need-type ((third body) 'string)
        (setf *level-name* (third body))
        (setf *level-name-color* (color-utils:byte-vector->vec4 (color (fourth body))))))))

(defun p-notes (body)
  (need-keyword ((first body) :set)
    (need-keyword ((second body) :notes)
      (need-type ((third body) 'string)
        (setf *level-notes* (third body))))))

(defun color (raw)
  (let ((token (symbol-name raw)))
    (if (color-prefix token)
        (vector (color-r token)
                (color-g token)
                (color-b token)
                (color-a token))
        (err +error-wrong-keyword+ +color-prefix+ token))))

(defun color-prefix (token)
  (let ((prefix (subseq token 0 (length +color-prefix+))))
    (string-equal prefix +color-prefix+)))

(defun color-any (token offset)
  (let ((c (subseq token offset (+ offset 2))))
    (parse-integer c :radix 16 :junk-allowed nil)))

(defun color-r (token)
  (color-any token (length +color-prefix+)))

(defun color-g (token)
  (color-any token 3))

(defun color-b (token)
  (color-any token 5))

(defun color-a (token)
  (color-any token 7))

(defun setup-seed ()
  (let ((cache-key (regular-file-strings->cache-key *raw-seed* +saved-seed-key+)))
    (if (cache-miss* cache-key)
        (with-open-file (stream cache-key :direction :output :if-does-not-exist :create
                                :if-exists :supersede)
          (setf *seed-after-loading* num:*lcg-seed*)
          (format stream "~a~%" *seed-after-loading*)
          #+debug-mode (misc:dbg "seed after loading ~a~%" *seed-after-loading*))
        (with-open-file (stream cache-key :direction :input :if-does-not-exist :error)
          (let ((seed (parse-integer (read-line stream))))
            (setf num:*lcg-seed* seed))))))

(defun branch-key-value (body)
  (if body
      (need-keyword-if ((first body) :set)
        (ecase (alexandria:make-keyword (second body))
          (:clouds
           (need-keyword-if ((third body) :random)
             (progn
               (setf *clouds* (set-value-random (fourth body)))
               (branch-key-value (subseq body 4)))
             (progn
               (set-key-value *clouds* (third body) 'symbol)
               (branch-key-value (subseq body 3)))))
          (:buildings-level
           (setf *building-level* (alexandria:clamp (truncate
                                                     (elt (process-parameter-list
                                                           (third body)) 0))
                                                    0 (length +available-level-wall+)))
           (branch-key-value (subseq body 3))))
        (need-keyword-if ((first body) :load)
          (progn
            (need-type ((second body) 'string)
              (load (res:get-resource-file (second body) +scripts-resource+))
              (branch-key-value (subseq body 2))))
          (need-keyword-if ((first body) :generate)
            (branch-generate body)
            (err "error input ~a" body))))))

(defmacro gen-simple-texture-generator (case-test offset &rest keywords)
  `(case ,case-test
     ,@(loop for i in keywords collect
            (let* ((raw-var (cl-ppcre:regex-replace-all "\\+" (symbol-name i) ""))
                   (keyword (alexandria:make-keyword raw-var)))
              `(,keyword (setf ,offset
                               (generate-texture
                                (subseq body 1)
                                ,keyword
                                ,(alexandria:format-symbol :texture (string-upcase i)))))))
     (otherwise
      (setf ,offset
            (generate-texture (subseq body 1)
                              (alexandria:make-keyword (second body))
                              (symbol-name (second body)))))))

(defun branch-generate (body)
  (if body
      (need-keyword-if ((first body) :generate)
        (let ((offset 0))
          (limited-progress)
          (case (alexandria:make-keyword (second body))
            (:skydome
             (setf offset (generate-skydome (subseq body 1))))
            (:cloud-1
             (setf offset (generate-clouds (subseq body 1) :cloud-1 texture:+cloud-1+)))
            (:cloud-2
             (setf offset (generate-clouds (subseq body 1) :cloud-2 texture:+cloud-2+)))
            (:cloud-3
             (setf offset (generate-clouds (subseq body 1) :cloud-3 texture:+cloud-3+)))
            (:map
             (setf offset (generate-map (subseq body 1))))
            (:tree
             (setf offset (generate-tree-w-cache (subseq body 1))))
            (:furniture
             (setf offset (generate-furniture (subseq body 1))))
            (:container
             (setf offset (generate-container-furniture (subseq body 1))))
            (:pillar
             (setf offset (generate-pillar-furniture (subseq body 1))))
            (:chair
             (setf offset (generate-chair-furniture (subseq body 1))))
            (:table
             (setf offset (generate-table-furniture (subseq body 1))))
            (:wall-decoration
             (setf offset (generate-wall-decoration-furniture (subseq body 1))))
            (:walkable
             (setf offset (generate-walkable-furniture (subseq body 1))))
            (:magic-furniture
             (setf offset (generate-magic-furniture (subseq body 1))))
            (:trap
             (setf offset (generate-trap (subseq body 1))))
            (:particle ;; generic-particle
             (setf offset
                   (generate-texture (subseq body 1) :particle (gen-particle-name))))
            (otherwise
             (gen-simple-texture-generator (alexandria:make-keyword (second body))
                                           offset
                                           +smoke-tray+
                                           +brick-wall+
                                           +dry-stone-wall+
                                           +grass+
                                           +grass-stones-floor+
                                           +stone-floor-road+
                                           +soil+
                                           +dry-soil+
                                           +wood-wall+
                                           +luxurious-floor-1+
                                           +luxurious-floor-2+
                                           +decal-wall-1+
                                           +decal-wall-2+
                                           +blood-splat+
                                           +blood-particle+
                                           +fire-particle+
                                           +smoke-particle+
                                           +cross-particle+
                                           +rock-1+
                                           +rock-2+
                                           +sand+
                                           +snow+)))
          (branch-generate (subseq body offset)))
        (need-keyword-if ((first body) :set)
          (branch-key-value body)
          (err "error input generate ~a" body)))))

(defun generate-skydome (body)
  (need-keyword ((first body) :skydome)
     (need-keyword ((second body) :from)
       (ecase (alexandria:make-keyword (third body))
         (:file
          (texture-from-file (elt body 3)
                             texture:+skydome-bg+
                             (elt body 6)
                             (elt body 9)
                             t)
          +offset-file+) ;; offset
         (:function
          (texture-from-function-w-cache (elt body 3) texture:+skydome-bg+ (list *game-hour*)
                                         (elt body 9) (elt body 12))
          +offset-function+);; offset
         (:string
          (texture-from-string (elt body 3) texture:+skydome-bg+ :tga (elt body 6)
                               (elt body 9))
          +offset-string+)))))

(defun generate-clouds (body check-if-eq target-db)
  (need-keyword ((first body) check-if-eq)
    (need-keyword ((second body) :from)
      (let ((density (case *clouds*
                       (:clear    '(10.0 22.0))
                       (:cloudy   '(22.0 50.0))
                       (:covered  '(50.0 70.0))
                       (otherwise '(10.0 22.0)))))
        (ecase (alexandria:make-keyword (third body))
          (:file
           (texture-from-file (elt body 3) target-db (elt body 6) (elt body 9) t)
           +offset-file+)
          (:function
           (texture-from-function-w-cache (elt body 3) target-db
                                          (append (elt body 6)
                                                  (list (num:lcg-next-in-range (first density)
                                                                               (second density))))
                                          (elt body 9)
                                          (elt body 12))
           +offset-function+)
          (:string
           (texture-from-string (elt body 3) target-db :tga (elt body 6) (elt body 9))
           +offset-string+))))))

(defun make-texture-cache-key (target)
  (ensure-cache-directory *raw-seed*)
  (regular-file-strings->cache-key *raw-seed* target))

(defun get-texture-from-cache-if-exists (key target-db tags normalmap-parameters)
  (if (not (cache-miss* key))
      (texture-from-file key target-db tags normalmap-parameters nil)
      nil))

(defun generate-texture (body check-if-eq target-db)
  (need-keyword ((first body) check-if-eq)
    (need-keyword ((second body) :from)
      (ecase (alexandria:make-keyword (third body))
        (:file
         (texture-from-file (elt body 3) target-db (elt body 6) (elt body 9) t)
         +offset-file+)
        (:function
         (let ((cache-key (make-texture-cache-key target-db)))
           (if (look-ahead-eq body 13 :and)
               (if (get-texture-from-cache-if-exists cache-key target-db (elt body 9)
                                                     (elt body 12))
                   (progn
                     (iterate-texture-from-function target-db :and (subseq body 13)
                                                    :just-consume-input t))
                   (progn
                     (texture-from-function (elt body 3) target-db (elt body 6) (elt body 9)
                                            (elt body 12))
                     (let ((offset (iterate-texture-from-function target-db
                                                                  :and (subseq body 13))))
                       (multiple-value-bind (texture-for-caching error)
                           (texture:get-texture target-db)
                         (if (not error)
                             (pixmap:save-pixmap texture-for-caching cache-key)
                             (err error)))
                       offset)))
               (texture-from-function-w-cache (elt body 3) target-db
                                              (elt body 6) (elt body 9)
                                              (elt body 12)))))
        (:string
         (texture-from-string (elt body 3) target-db :tga (elt body 6) (elt body 9))
         +offset-string+)))))

(defun iterate-texture-from-function (target keyword body &key (offset 0) (just-consume-input nil))
  (need-keyword-if ((first body) keyword) ;; and with postprocess
     (need-keyword-if ((second body) :with)
        (need-keyword-if ((third body) :postprocess)
           (multiple-value-bind (tx error)
               (texture:get-texture target)
             (if (not error)
                 (progn
                   (when (not just-consume-input)
                     (funcall (fourth body) tx)) ;; the function
                   (iterate-texture-from-function target :and
                                                  (subseq body +offset-postprocess+)
                                                  :offset
                                                  (+ +offset-function+ +offset-postprocess+)
                                                  :just-consume-input just-consume-input))
                 (and error (first error))))
           nil)
        nil)
     offset))

(defun texture-from-file (path target tags normalmap-parameters as-resource)
  (if as-resource
      (texture-from-file-as-resource path target tags normalmap-parameters)
      (texture-from-file-abs         path target tags normalmap-parameters)))

(defun plist->normalmap-params (plist)
  (mesh-material:make-mesh-material (getf plist :ka        1.0)
                                    (getf plist :kd        1.0)
                                    (getf plist :ks        1.0)
                                    (getf plist :roughness 1.0)
                                    (getf plist :shininess 128.0)))

(defun texture-from-file-as-resource (path target tags normalmap-parameters)
  (need-type (path 'string)
    (multiple-value-bind (bg error)
        (texture:get-texture (res:get-resource-file path +textures-resource+))
      (if (not error)
          (progn
            (texture:prepare-for-rendering bg)
            (setf (texture:filename bg) target)
            (setf (texture:tags bg) (map 'vector #'identity tags))
            (setf (texture:normalmap-params bg)
                  (plist->normalmap-params (process-parameter-list normalmap-parameters))))
          (first error)))))

(defun texture-from-file-abs (path target tags normalmap-parameters)
  (need-type (path 'string)
    (multiple-value-bind (bg error)
        (texture:get-texture path)
      (if (not error)
          (progn
            (texture:prepare-for-rendering bg)
            (setf (texture:filename bg) target)
            (setf (texture:tags bg) (map 'vector #'identity tags))
            (setf (texture:normalmap-params bg)
                  (plist->normalmap-params (process-parameter-list normalmap-parameters))))
          (first error)))))

(defun texture-from-function (function-name target args tags normalmap-parameters)
  (need-type (function-name 'symbol)
    (let* ((pixmap  (apply (symbol-function function-name)
                           (process-parameter-list args)))
           (texture (texture:gen-name-and-inject-in-database pixmap)))
      (if texture
          (progn
            (setf (texture:filename texture) target)
            (setf (texture:tags texture) (map 'vector #'identity tags))
            (setf (texture:normalmap-params texture)
                  (plist->normalmap-params (process-parameter-list normalmap-parameters)))
            (texture:prepare-for-rendering texture)
            texture)
          (err +error-cannot-create-texture-from-function+ function-name)))))

(defun texture-from-function-w-cache (function-name target args tags normalmap-parameters)
  (let ((cache-key (make-texture-cache-key target)))
    (or (get-texture-from-cache-if-exists cache-key target tags normalmap-parameters)
        (pixmap:save-pixmap (texture-from-function function-name target
                                                   args tags normalmap-parameters)
                            cache-key))
    +offset-function+))

(defun texture-from-string (data target type tags normalmap-parameters)
  (need-type (data 'string)
    (let* ((pixmap (ecase type
                       (:tga
                        (let ((p    (make-instance 'pixmap:tga))
                              (data (b64:decode data)))
                          (pixmap:load-from-vector p data)
                          p))))
           (texture (texture:gen-name-and-inject-in-database pixmap)))
      (if texture
          (progn
            (setf (texture:filename texture) target)
            (setf (texture:tags texture) (map 'vector #'identity tags))
            (setf (texture:normalmap-params texture)
                  (plist->normalmap-params (process-parameter-list normalmap-parameters)))
            (texture:prepare-for-rendering texture))
          (err +error-cannot-create-texture-from-string+ data)))))

(defmacro need-phrase ((config keywords pos) &body body)
   (if keywords
       `(need-keyword ((elt ,config 0) ,(first keywords))
         (need-phrase ((rest ,config) ,(rest keywords) ,(1+ pos)) ,@body))
       `(progn ,@body)))


;(need-phrase (body (:map :with :size) 0))

(defun keyword-to-int (keywords ints key default)
  (assert (= (length keywords) (length ints)))
  (let ((pos (position key keywords :test #'eq)))
    (if pos
        (elt ints pos)
        default)))

(defun get-fn-parameter-for-map (body &optional (fns '()) (offset 0))
  (need-keyword-if ((first body) :with)
   (let ((fn-name (second body))
         (fn      (third  body)))
     (get-fn-parameter-for-map (subseq body 3)
                               (push (cons fn-name
                                           (cond
                                             ((symbolp fn)
                                              (symbol-function fn))
                                             ((functionp fn)
                                              fn)
                                             (t ;if fn is a number this will be a proper list!
                                              (process-parameter-list fn))))
                                     fns)
                               (+ offset 3)))
    (values fns offset)))

(defun make-map-cache-key ()
  (ensure-cache-directory *raw-seed*)
  (regular-file-strings->cache-key *raw-seed* +map-cache-name+))

(defun generate-map (body)
  (let ((offset 0))
     (need-phrase (body (:map :with :size) 0)
       (let* ((size (keyword-to-int  '(:tiny :small :medium :large)
                                     (list (truncate +tiny-map-size+)   ; 16
                                           (truncate +small-map-size+)  ; 32
                                           (truncate +medium-map-size+) ; 48
                                           (truncate +large-map-size+)) ; 64
                                     (first (process-parameter-list (elt body 3)))
                                     64))
              (map (make-instance 'random-terrain:random-terrain :matrix (gen-empty-terrain size size))))
         (multiple-value-bind (fns offset-fn)
             (get-fn-parameter-for-map (subseq body 4))
           (let ((mountain-rate (or (cdr (assoc 'mountain-rate fns)) '(0.05)))
                 (mountain-z-height-function
                  (or (cdr (assoc 'mountain-z-height-function fns))
                      #'random-terrain:default-mountain-z-height-function))
                 (mountain-w-function
                  (or (cdr (assoc 'mountain-w-function fns))
                      #'random-terrain:default-mountain-size-function))
                 (mountain-h-function
                  (or (cdr (assoc 'mountain-h-function fns))
                      #'random-terrain:default-mountain-size-function))
                 (mountain-sigma-w-function
                  (or (cdr (assoc 'mountain-sigma-w-function fns))
                      #'random-terrain:default-mountain-sigma-function))
                 (mountain-sigma-h-function
                  (or (cdr (assoc 'mountain-sigma-h-function fns))
                      #'random-terrain:default-mountain-sigma-function))
                 (lake-rate (or (cdr (assoc 'lake-rate fns)) '(0.05)))
                 (lake-size-function
                  (or (cdr (assoc 'lake-size-function fns))
                      #'random-terrain:default-lake-size-function))
                 (labyrinth-rate (or (cdr (assoc 'labyrinth-rate fns))
                                     '(0.1)))
                 (labyrinth-size-function
                  (or (cdr (assoc 'labyrinth-size-function fns))
                      #'random-terrain:default-labyrinth-size-function))
                 (labyrinth-sigma-w-function
                  (or (cdr (assoc 'labyrinth-sigma-w-function fns))
                      #'random-terrain:default-labyrinth-sigma-w-function))
                 (labyrinth-sigma-h-function
                  (or (cdr (assoc 'labyrinth-sigma-h-function fns))
                      #'random-terrain:default-labyrinth-sigma-h-function))
                 (labyrinth-door-function
                  (or (cdr (assoc 'labyrinth-door-function fns))
                      #'random-terrain:default-labyrinth-door-function))
                 (labyrinth-win-function
                  (or (cdr (assoc 'labyrinth-win-function fns))
                      #'random-terrain:default-labyrinth-win-function))
                 (labyrinth-furniture-function
                  (or (cdr (assoc ' labyrinth-furniture-function fns))
                      #'random-terrain:default-labyrinth-furniture-function))
                 (soil-decal-threshold (or (cdr (assoc 'soil-decal-threshold fns))
                                           '(0.8)))
                 (trees-rate  (or (cdr (assoc 'trees-rate fns))
                                  '(0.01)))
                 (trees-sparseness  (or (cdr (assoc 'trees-sparseness fns))
                                        '(0.8))))
             (let ((cache-key (make-map-cache-key)))
               (if (cache-miss* cache-key)
                   (progn
                     (setf *map*
                           (random-terrain:make-map
                            map
                            :mountain-rate              (elt mountain-rate 0)
                            :mountain-z-height-function (funcall mountain-z-height-function)
                            :mountain-w-function        (funcall mountain-w-function)
                            :mountain-h-function        (funcall mountain-h-function)
                            :mountain-sigma-w-function  (funcall mountain-sigma-w-function)
                            :mountain-sigma-h-function  (funcall mountain-sigma-h-function)
                            :lake-rate                  (elt lake-rate 0)
                            :lake-size-function         (funcall lake-size-function)
                            :labyrinth-rate             (elt labyrinth-rate 0)
                            :labyrinth-size-function    (funcall labyrinth-size-function)
                            :labyrinth-sigma-w-function (funcall labyrinth-sigma-w-function)
                            :labyrinth-sigma-h-function (funcall labyrinth-sigma-h-function)
                            :labyrinth-door-function    (funcall labyrinth-door-function)
                            :labyrinth-win-function     (funcall labyrinth-win-function)
                            :labyrinth-furn-function    (funcall labyrinth-furniture-function)
                            :soil-threshold             (elt soil-decal-threshold 0)
                            :trees-rate                 (elt trees-rate 0)
                            :trees-sparseness           (elt trees-sparseness 0)))
                     (filesystem-utils:dump-sequence-to-file (random-terrain:serialize *map*)
                                                             cache-key))
                   (setf *map*
                         (random-terrain:deserialize (make-instance 'random-terrain:random-terrain)
                                                     cache-key))))
             (incf offset (+ 5 offset-fn))))))))

(defun tree-from-dump (path as-resource)
  (interfaces:deserialize (make-instance 'mesh:triangle-mesh)
                          (if as-resource
                              (res:get-resource-file path
                                                     constants:+trees-resource+
                                                     :if-does-not-exists :error)
                              path)))

(defun string->cache-key (s)
  (ensure-cache-directory *raw-seed*)
  (regular-file-strings->cache-key *raw-seed*
                                   (format nil "~a" (num:string-fnv-hash-32 s))))

(defun make-tree-dump-cache-key (path)
  (string->cache-key path))

(defun %gen-tree (file material)
  (let ((normalmap-parameters (plist->normalmap-params
                               (process-parameter-list material)))
        (mesh (trees:gen-tree (res:get-resource-file file
                                                     constants:+trees-resource+
                                                     :if-does-not-exists :error)
                              :flatten t)))
    (setf (mesh:material-params mesh) normalmap-parameters)
    mesh))

(defun generate-tree-w-cache (body)
  (let ((offset 0))
    (need-phrase (body (:tree :from) 0)
      (ecase (alexandria:make-keyword (third body))
        (:script
         (let ((cache-key (make-tree-dump-cache-key (fourth body))))
           (push (mesh:prepare-for-rendering
                  (if (not (cache-miss* cache-key))
                      (tree-from-dump cache-key nil)
                      (%gen-tree (fourth body) (elt body 6))))
                 *trees*)
           (when (cache-miss* cache-key)
             (mesh:save-mesh (first *trees*) cache-key))
           (setf offset 8)))
        (:file
         (if (filesystem-utils:has-extension (fourth body) +obj-mesh-file-extension+)
             (let ((mesh (load-obj-mesh constants:+trees-resource+
                                        (fourth body)
                                        (elt body 6))))
               (setf (texture:use-mipmap (mesh:texture-object mesh)) t)
               (push mesh *trees*))
             (err (format nil "tree mesh: file-not-supported: ~a" (fourth body))))
         (setf offset 8))
        (:dump
         (push (tree-from-dump (fourth body) t) *trees*)
         (setf offset 5)))
    offset)))

(defmacro %load-mesh (body bag error-message &optional (res '+furnitures-resource+))
  (alexandria:with-gensyms (mesh)
    `(progn
       (if (filesystem-utils:has-extension (fourth ,body) +obj-mesh-file-extension+)
           (let* ((,mesh (load-obj-mesh ,res
                                        (fourth ,body)
                                        (elt ,body 6))))
             (push ,mesh ,bag))
           (err ,error-message (fourth ,body)))
       8)))

(defun generate-trap (body)
  (%load-mesh body
              *trap*
              "Land mine furniture mesh: file not supported: ~a"
              +model-objects-resource+))

(defun generate-furniture (body)
  (%load-mesh body *furnitures* "Furniture mesh: file not supported: ~a"))

(defun generate-container-furniture (body)
  (%load-mesh body
              *containers-furnitures*
              "Container furniture mesh: file not supported: ~a"))

(defun generate-magic-furniture (body)
  (%load-mesh body
              *magic-furnitures*
              "Magic furniture mesh: file not supported: ~a"))

(defun generate-pillar-furniture (body)
  (%load-mesh body
              *pillar-furnitures*
              "Pillar furniture mesh: file not supported: ~a"))

(defun generate-chair-furniture (body)
  (%load-mesh body
              *chair-furnitures*
              "Chair furniture mesh: file not supported: ~a"))

(defun generate-table-furniture (body)
  (%load-mesh body
              *table-furnitures*
              "Table furniture mesh: file not supported: ~a"))

(defun generate-wall-decoration-furniture (body)
  (%load-mesh body
              *wall-decoration-furnitures*
              "Wall furniture mesh: file not supported: ~a"))

(defun generate-walkable-furniture (body)
  (%load-mesh body
              *walkable-furnitures*
              "Walkable furniture mesh: file not supported: ~a"))

(defun load-obj-mesh (resource file material)
  (let* ((normalmap-parameters (plist->normalmap-params
                                (process-parameter-list material)))
         (mesh  (obj-mesh:load (res:get-resource-file file
                                                      resource
                                                      :if-does-not-exists :error))))
    (setf (mesh:material-params mesh) normalmap-parameters)
    mesh))
