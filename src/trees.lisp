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

(in-package :trees)

(alexandria:define-constant +lsys-comment-line+ "#.*\\n|^\\p{Z}*\\n" :test 'string=)

(defclass turtle ()
  ((x-axe
    :initform +x-axe+
    :initarg :x-axe
    :accessor x-axe)
   (y-axe
    :initform +y-axe+
    :initarg :y-axe
    :accessor y-axe)
   (z-axe
    :initform +z-axe+
    :initarg :z-axe
    :accessor z-axe)
   (direction
    :initform +y-axe+
    :initarg :direction
    :accessor direction
    :type vector)
   (old-direction
    :initform +y-axe+
    :initarg :old-direction
    :accessor old-direction
    :type vector)))

(defgeneric rotate-turtle (object angle &key axe))

(defgeneric rotate-turtle-arbitrary-axe (object angle &key axe))

(defgeneric clone-turtle (object))

(defgeneric get-axe (object axe-keyword))

(defmethod rotate-turtle ((object turtle) (angle float) &key (axe :x))
  (let* ((pivot (ecase axe
                 (:x (x-axe object))
                 (:y (y-axe object))
                 (:z (z-axe object))))
         (rotation-mat (rotate-around pivot (coerce (deg->rad angle) 'single-float))))
    (ecase axe
      (:x
       (setf (y-axe object) (transform-direction (y-axe object) rotation-mat))
       (setf (z-axe object) (transform-direction (z-axe object) rotation-mat)))
      (:y
       (setf (x-axe object) (transform-direction (x-axe object) rotation-mat))
       (setf (z-axe object) (transform-direction (z-axe object) rotation-mat)))
      (:z
       (setf (x-axe object) (transform-direction (x-axe object) rotation-mat))
       (setf (y-axe object) (transform-direction (y-axe object) rotation-mat))))
    (setf (direction object) (transform-direction (direction object) rotation-mat))))

(defmethod rotate-turtle-arbitrary-axe ((object turtle) (angle float)
                                        &key (axe (vec 0.0 1.0 0.0)))

  (let* ((rotation-mat (rotate-around axe (coerce (deg->rad angle) 'single-float))))
    (setf (x-axe object) (transform-direction (x-axe object) rotation-mat))
    (setf (y-axe object) (transform-direction (y-axe object) rotation-mat))
    (setf (z-axe object) (transform-direction (z-axe object) rotation-mat))
    (setf (direction object) (transform-direction (direction object) rotation-mat))))

(defmethod clone-turtle ((object turtle))
  (with-accessors ((x x-axe) (y y-axe) (z z-axe)
                   (direction direction)) object
    (make-instance 'turtle
                   :x-axe (copy-vec x)
                   :y-axe (copy-vec y)
                   :z-axe (copy-vec z)
                   :direction (copy-vec direction))))

(defmethod get-axe ((object turtle) axe-keyword)
  (ecase axe-keyword
    (:x (x-axe object))
    (:y (y-axe object))
    (:z (z-axe object))))

(defun make-turtle (&optional (x-axe +x-axe+) (y-axe +y-axe+) (z-axe +z-axe+)
                    (direction +y-axe+))
  (make-instance 'turtle
                   :x-axe x-axe
                   :y-axe y-axe
                   :x-axe z-axe
                   :direction direction))

(defclass rule ()
  ((chance
    :initform 1.0
    :initarg :chance
    :accessor chance)
   (expression
    :initform nil
    :initarg :expression
    :accessor expression)))

(defmethod print-object ((object rule) stream)
  (format stream "chance ~s expression ~s" (chance object) (expression object)))

(defclass rules-set ()
  ((rules
    :initform '()
    :initarg :rules
    :accessor rules
    :type rule)))

(defmethod print-object ((object rules-set) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "rules~%~{~a~%~}" (rules object))))

(defgeneric choose-rule (object))

(defgeneric sort-ruleset (object))

(defgeneric clean-chances-ruleset (object))

(defmethod choose-rule ((object rules-set))
  (with-accessors ((rules rules)) object
    (let* ((dice-roll (lcg-next01))
           (chosen (do* ((rule rules (rest rule))
                         (cumulative-chance (chance (first rule))
                                            (+ cumulative-chance (chance (first rule)))))
                        ((not (< cumulative-chance dice-roll)) (first rule)))))
      chosen)))

(defmethod sort-ruleset ((object rules-set))
  (setf (rules object)
        (sort (rules object) #'< :key #'(lambda (r) (chance r)))))

(defmethod clean-chances-ruleset ((object rules-set))
  (let ((chance-sum (loop for r in (rules object) sum (chance r))))
    (when (or (< chance-sum 1.0)
              (> chance-sum 1.0))
      (loop for r in (rules object) do
           (setf (chance r) (float (/ 1 (length (rules object)))))))))

(defclass section-parameters ()
  ((id
    :initform -1
    :initarg :id
    :accessor id)
   (radius
    :initform (desired -1)
    :initarg :radius
    :accessor radius)
   (parameters-parent
    :initform nil
    :initarg :parameters-parent
    :accessor parameters-parent)))

(defclass lsys-parsed-file (renderizable parsed-file)
  ((mesh
    :initform (make-instance 'triangle-mesh)
    :initarg :mesh
    :accessor mesh)
   (current-node
    :initform nil
    :initarg :current-node
    :accessor current-node)))

(defmethod initialize-instance :after ((object lsys-parsed-file) &key &allow-other-keys)
  (with-slots (comment-line current-node mesh) object
    (setf comment-line +lsys-comment-line+
          current-node mesh)))

(alexandria:define-constant +integer+ "0|[1-9][0-9]+|[1-9]" :test 'string=)

(alexandria:define-constant +float+ "-?[0-9]+(\\.[0-9]+)?" :test 'string=)

(alexandria:define-constant +pop+ "\\)" :test 'string=)

(alexandria:define-constant +push+ "\\(" :test 'string=)

(defmacro gen-push-constant (&rest attributes)
  `(progn
     ,@(loop for i in attributes collect
            `(alexandria:define-constant
                 ,(alexandria:format-symbol t "~:@(+push-~a+~)" (first i))
                 ,(string-downcase (symbol-name (second i)))
               :test #'string=))))

(gen-push-constant (all a) (color c) (direction d) (rotation-angle g)
                   (length l) (position p) (radius r)
                   (texture-coordinate t)
                   (rotation-axes x))

(alexandria:define-constant +root-rule-label+ "ROOT" :test #'string=)

(alexandria:define-constant +bending-factor-label+ "bending-factor" :test #'string=)

(alexandria:define-constant +tropism-label+ "tropism" :test #'string=)

(alexandria:define-constant +texture-file-label+ "texture" :test #'string=)

(alexandria:define-constant +color-definition-label+ "colors" :test #'string=)

(alexandria:define-constant +depth-definition-label+ "depth" :test #'string=)

(alexandria:define-constant +length-definition-label+ "length" :test #'string=)

(alexandria:define-constant +angle-definition-label+ "angle" :test #'string=)

(alexandria:define-constant +radius-definition-label+ "radius" :test #'string=)

(alexandria:define-constant +randomize-length-label+ "randomize-length" :test #'string=)

(alexandria:define-constant +randomize-angle-label+ "randomize-angle" :test #'string=)

(alexandria:define-constant +polygon-magnetic-range+ "polygon-magnetic-range" :test #'string=)

(alexandria:define-constant +section-magnetic-range+ "section-magnetic-range" :test #'string=)

(alexandria:define-constant +rule-stopper+ ";" :test #'string=)

(alexandria:define-constant +rotate-positive+ "\\+" :test #'string=)

(alexandria:define-constant +rotate-negative+ "-" :test #'string=)

(alexandria:define-constant +draw+ "\\." :test #'string=)

(alexandria:define-constant +move+ ":" :test #'string=)

(alexandria:define-constant +delete-branch+ "\\$" :test #'string=)

(alexandria:define-constant +select-x+ "x" :test #'string=)

(alexandria:define-constant +select-y+ "y" :test #'string=)

(alexandria:define-constant +select-z+ "z" :test #'string=)

(alexandria:define-constant +next-rotation-axe+ "/" :test #'string=)

(alexandria:define-constant +previous-rotation-axe+ "\\" :test #'string=)

(alexandria:define-constant +next-color+ "n" :test #'string=)

(alexandria:define-constant +reset-pos+ "s" :test #'string=)

(alexandria:define-constant +reset-direction+ "rd" :test #'string=)

(alexandria:define-constant +enter-polygon-mode+ "m" :test #'string=)

(alexandria:define-constant +conditional+ "if" :test #'string=)

(alexandria:define-constant +<+ "<" :test #'string=)

(alexandria:define-constant +>+ ">" :test #'string=)

(alexandria:define-constant +>=+ ">=" :test #'string=)

(alexandria:define-constant +<=+ "<=" :test #'string=)

(alexandria:define-constant +bool-radius-label+ "r" :test #'string=)

(alexandria:define-constant +bool-length-label+ "l" :test #'string=)

(alexandria:define-constant +scale-length+ "%l=" :test #'string=)

(alexandria:define-constant +scale-radius+ "%r=" :test #'string=)

(alexandria:define-constant +scale-angle+ "%a=" :test #'string=)

(alexandria:define-constant +scale-tropism-bending-factor+ "%t=" :test #'string=)

(alexandria:define-constant +scale-magnetic-polygon-range+ "%m=" :test #'string=)

(alexandria:define-constant +scale-section-magnetic-range+ "%j=" :test #'string=)

(alexandria:define-constant +color+ "c=" :test #'string=)

(alexandria:define-constant +length+ "l=" :test #'string=)

(alexandria:define-constant +angle+ "a=" :test #'string=)

(alexandria:define-constant +radius+ "r=" :test #'string=)

(alexandria:define-constant +s-coord+ "s=" :test #'string=)

(alexandria:define-constant +t-coord+ "t=" :test #'string=)

(alexandria:define-constant +incr-s-coord+ "incr_s=" :test #'string=)

(alexandria:define-constant +incr-t-coord+ "incr_t=" :test #'string=)

(alexandria:define-constant +s-coord-section+ "sec_s=" :test #'string=)

(alexandria:define-constant +t-coord-section+ "sec_t=" :test #'string=)

(alexandria:define-constant +t-coord-section-end+ "sec_t_end=" :test #'string=)

(alexandria:define-constant +incr-s-coord-section+ "sec_incr_s=" :test #'string=)

(alexandria:define-constant +incr-t-coord-section+ "sec_incr_t=" :test #'string=)

(alexandria:define-constant +incr-t-coord-section-end+ "sec_incr_t_end=" :test #'string=)

(alexandria:define-constant +chance-param-label+ "chance=" :test #'string=)

(alexandria:define-constant +eol+ "\\n" :test #'string=)

(alexandria:define-constant +colors-separator+ "," :test #'string=)

(alexandria:define-constant +equal+ "=" :test #'string=)

(alexandria:define-constant +rule-names+ "[A-Z,_]+" :test #'string=)

(alexandria:define-constant +inline-label+ "inline" :test #'string=)

(alexandria:define-constant +section-s-coord-offset+ 0.25 :test #'=)

(define-tokenizer (lsys-parsed-file
                   +inline-label+
                   +delete-branch+
                   +bending-factor-label+
                   +tropism-label+
                   +texture-file-label+
                   +polygon-magnetic-range+
                   +section-magnetic-range+
                   +chance-param-label+
                   +rotate-positive+ +rotate-negative+
                   +draw+ +move+ +select-x+ +select-y+
                   +select-z+ +next-rotation-axe+
                   +previous-rotation-axe+ +next-color+ +reset-pos+
                   +reset-direction+
                   +color+
                   +scale-length+ +scale-radius+ +scale-angle+
                   +scale-tropism-bending-factor+
                   +scale-magnetic-polygon-range+
                   +scale-section-magnetic-range+
                   +length+ +angle+ +radius+
                   +s-coord-section+
                   +t-coord-section+
                   +t-coord-section-end+
                   +incr-s-coord-section+
                   +incr-t-coord-section+
                   +incr-t-coord-section-end+
                   +s-coord+ +t-coord+
                   +incr-s-coord+ +incr-t-coord+
                   +eol+ +equal+ +integer+ +float+
                   +colors-separator+ +push+ +push-all+ +push-color+
                   +push-direction+ +push-rotation-angle+
                   +push-length+ +push-position+
                   +push-radius+ +push-rotation-axes+
                   +push-texture-coordinate+
                   +pop+
                   +rule-stopper+ +color-definition-label+
                   +depth-definition-label+
                   +length-definition-label+
                   +randomize-length-label+
                   +angle-definition-label+
                   +randomize-angle-label+
                   +radius-definition-label+
                   +conditional+
                   +<+ +>+ +<=+ +>=+
                   +enter-polygon-mode+
                   +rule-names+)
  ((member (char@) *blank-space* :test #'string=)
   (buffered-input-file:increment-pointer *file*)
   (next-token *file*)))

(define-is-stuff-p cl-ppcre:scan
    +inline-label+
  +delete-branch+
  +bending-factor-label+
  +tropism-label+
  +texture-file-label+
  +polygon-magnetic-range+
  +section-magnetic-range+
  +chance-param-label+
  +rotate-positive+ +rotate-negative+ +draw+ +move+
  +select-x+ +select-y+ +select-z+ +next-rotation-axe+
  +previous-rotation-axe+ +next-color+ +reset-pos+
  +reset-direction+
  +color+
  +scale-length+ +scale-radius+ +scale-angle+
  +scale-tropism-bending-factor+
  +scale-magnetic-polygon-range+
  +scale-section-magnetic-range+
  +length+ +angle+ +radius+
  +s-coord-section+
  +t-coord-section+
  +t-coord-section-end+
  +incr-s-coord-section+
  +incr-t-coord-section+
  +incr-t-coord-section-end+
  +s-coord+ +t-coord+
  +incr-s-coord+ +incr-t-coord+
  +eol+ +equal+ +colors-separator+
  +push+
  +push-all+ +push-color+
  +push-direction+ +push-rotation-angle+
  +push-length+ +push-position+
  +push-radius+ +push-rotation-axes+
  +push-texture-coordinate+
  +integer+ +float+ +rule-names+ +pop+
  +rule-stopper+ +color-definition-label+
  +depth-definition-label+
  +length-definition-label+
  +randomize-length-label+
  +angle-definition-label+
  +randomize-angle-label+
  +radius-definition-label+
  +enter-polygon-mode+
  +conditional+
  +<+ +>+ +<=+ +>=+)

(defmacro test-all (token &body constants)
  `(or
    ,@(loop for i in constants collect
           `(,(alexandria:format-symbol t "IS-~a-P"
                                        (subseq (subseq (symbol-name i) 1)
                                                0 (1- (length (subseq (symbol-name i) 1)))))
              ,token))))

(defun is-opcode-p (token)
  (test-all token
    +delete-branch+
    +rotate-positive+ +rotate-negative+ +draw+ +move+
    +select-x+ +select-y+ +select-z+ +next-rotation-axe+
    +previous-rotation-axe+ +next-color+ +reset-pos+
    +reset-direction+
    +color+
    +scale-length+ +scale-radius+ +scale-angle+
    +scale-tropism-bending-factor+
    +scale-magnetic-polygon-range+
    +scale-section-magnetic-range+
    +length+ +angle+ +radius+
    +s-coord-section+
    +t-coord-section+
    +incr-s-coord-section+
    +incr-t-coord-section+
    +t-coord-section-end+
    +incr-t-coord-section-end+
    +s-coord+ +t-coord+
    +incr-s-coord+ +incr-t-coord+
    +push-texture-coordinate+
    +push-all+ +push-color+
    +push-direction+ +push-rotation-angle+
    +push-length+ +push-position+
    +push-radius+ +push-rotation-axes+
    +enter-polygon-mode+
    +conditional+))

(defun is-opcode-with-argument-p (token)
  (test-all token +color+
            +incr-s-coord+ +incr-t-coord+
            +scale-tropism-bending-factor+
            +scale-length+ +scale-radius+ +scale-angle+
            +scale-magnetic-polygon-range+
            +scale-section-magnetic-range+
            +length+ +angle+ +radius+
            +s-coord-section+
            +t-coord-section+
            +t-coord-section-end+
            +incr-s-coord-section+
            +incr-t-coord-section+
            +incr-t-coord-section-end+
            +s-coord+ +t-coord+
            +incr-s-coord+ +incr-t-coord+))

(defun is-push-opcode-p (token)
  (test-all token +push-all+ +push-color+
            +push-direction+ +push-rotation-angle+
            +push-length+ +push-position+
            +push-radius+ +push-rotation-axes+
            +push-texture-coordinate+))


(defun push-opcode-makes-physical-branch-p (token)
  (test-all token +push-all+ +push-position+))

(defparameter *current-rotation-axe* :z)

(defparameter *current-color* '(1.0 1.0 1.0 1.0))

(defparameter *current-direction* (make-turtle))

(defparameter *current-rotation-angle* 0.0)

(defparameter *current-length* 1.0)

(defparameter *length-randomize-rate* 0.0)

(defparameter *angle-randomize-rate* 0.0)

(defparameter *current-position* (vec 0.0 0.0 0.0))

(defparameter *current-radius* .1)

(defparameter *current-bending-factor* 0)

(defparameter *current-tropism* (vec 0.0 -1.0 0.0))

(defparameter *current-texture* "")

(defparameter *current-polygon-magnetic-range* 2e-1)

(defparameter *current-fill-gap-treshold* 0.3)

(defparameter *current-s-coord* 0.0)

(defparameter *current-t-coord* 0.0)

(defparameter *current-s-coord-section* 0.0)

(defparameter *current-t-coord-section* 0.0)

(defparameter *current-t-coord-section-end* 0.0)

(defparameter *stack-rotation-axe* '())

(defparameter *stack-bending-factor* '())

(defparameter *stack-color* '())

(defparameter *stack-direction* '())

(defparameter *stack-rotation-angle* '())

(defparameter *stack-length* '())

(defparameter *stack-position* '())

(defparameter *stack-radius* '())

(defparameter *stack-s-coord* '())

(defparameter *stack-t-coord* '())

(defparameter *stack-s-coord-section* '())

(defparameter *stack-t-coord-section* '())

(defparameter *stack-t-coord-section-end* '())

(defparameter *stack* '())

(defparameter *rules* (make-hash-table :test #'equal))

(defparameter *palette* '())

(defparameter *palette-pointer* 0)

(defparameter *max-depth* 4)

(defparameter *depth* 0)

(defparameter *polygon-mode* nil)

(defparameter *current-polygon* nil)

(define-parser-skeleton trees lsys lsys-parsed-file
                        (*current-bending-factor* 0)
                        (*current-tropism* (vec 0.0 -1.0 0.0))
                        (*current-texture* "")
                        (*current-polygon-magnetic-range* 2e-1)
                        (*current-fill-gap-treshold* 0.3)
                        (*current-rotation-axe* :z)
                        (*current-color* (vec 1.0 1.0 1.0))
                        (*current-direction* (make-turtle))
                        (*current-rotation-angle* 0.0)
                        (*current-length* 10.0)
                        (*current-s-coord* 0.75)
                        (*current-t-coord* 0.0)
                        (*current-s-coord-section* 0.0)
                        (*current-t-coord-section* 0.0)
                        (*current-t-coord-section-end* 1.0)
                        (*length-randomize-rate* 0.0)
                        (*angle-randomize-rate* 0.0)
                        (*current-position* (vec 0.0 0.0 0.0))
                        (*current-radius* 1.0)
                        (*stack-rotation-axe* '())
                        (*stack-bending-factor* '())
                        (*stack-color* '())
                        (*stack-direction* '())
                        (*stack-rotation-angle* '())
                        (*stack-length* '())
                        (*stack-position* '())
                        (*stack-radius* '())
                        (*stack-s-coord* '())
                        (*stack-t-coord* '())
                        (*stack* '())
                        (*rules* (make-hash-table :test #'equal))
                        (*palette* '())
                        (*max-depth* 4)
                        (*depth* 0)
                        (*polygon-mode* nil))

(defun next-rotation-axe (current)
  (ecase current
    (:x :y)
    (:y :z)
    (:z :x)))

(defun previous-rotation-axe (current)
  (ecase current
    (:x :z)
    (:z :y)
    (:y :x)))

(defun next-palette-color ()
  (let ((next-color-index (mod (1+ *palette-pointer*) (length *palette*))))
    (setf *palette-pointer* next-color-index)
    (nth *palette-pointer* *palette*)))

(defun previous-palette-color ()
  (if (= 0 *palette-pointer*)
      (progn
        (setf *palette-pointer* (1- (length *palette*)))
        (alexandria:lastcar *palette*))
      (progn
        (decf *palette-pointer*)
        (nth  *palette-pointer* *palette*))))

(defun select-palette-color (index)
  (if (null (nth (truncate index) *palette*))
      (first *palette*)
      (nth (truncate index) *palette*)))

(defmethod parse-comment-line ((object lsys-parsed-file))
  (with-no-errors
    (multiple-value-bind (line length start)
        (buffered-input-file:get-line *file*)
      (declare (ignore length))
      (if (is-comment-line-p object line)
          (parse-comment-line object)
          (progn
            (buffered-input-file:seek *file* start)
            nil)))))

(defnocfun parse-system ()
  (parse-depth)
  (parse-length)
  (parse-randomize-length-rate)
  (parse-angle)
  (parse-randomize-angle-rate)
  (parse-radius)
  (parse-color-def)
  (parse-bending-factor)
  (parse-tropism)
  (parse-texture)
  (parse-polygon-magnetic-range)
  (parse-section-magnetic-range)
  (parse-axioms))

(defmacro gen-parse-key-float-value-entry ((name var validator))
  (alexandria:with-gensyms (token)
    `(defnocfun ,(alexandria:format-symbol t "~:@(parse-~a~)" name) ()
       (with-no-errors
           (let ((,token (peek-token *file*)))
             (cond
               ((,validator ,token)
                (next-token *file*)
                (,(alexandria:format-symbol t "~:@(parse-~a~)" name)))
               ((is-float-p ,token)
                ,(if var
                     `(setf ,var (parse-number->desired (next-token *file*)))
                     `(parse-number->desired (next-token *file*))))))))))

(gen-parse-key-float-value-entry (bending-factor *current-bending-factor*
                                                 is-bending-factor-label-p))

(gen-parse-key-float-value-entry (chance-param nil is-chance-param-label-p))

(gen-parse-key-float-value-entry (depth *max-depth* is-depth-definition-label-p))

(gen-parse-key-float-value-entry (length *current-length* is-length-definition-label-p))

(gen-parse-key-float-value-entry (radius *current-radius* is-radius-definition-label-p))

(gen-parse-key-float-value-entry (angle *current-rotation-angle* is-angle-definition-label-p))

(gen-parse-key-float-value-entry (randomize-length-rate *length-randomize-rate*
                                                        is-randomize-length-label-p))

(gen-parse-key-float-value-entry (randomize-angle-rate *angle-randomize-rate*
                                                       is-randomize-angle-label-p))

(gen-parse-key-float-value-entry (polygon-magnetic-range *current-polygon-magnetic-range*
                                                         is-polygon-magnetic-range-p))

(gen-parse-key-float-value-entry (section-magnetic-range
                                  *current-fill-gap-treshold*
                                  is-section-magnetic-range-p))


(defmacro gen-parse-key-string-value-entry ((name var validator))
  (alexandria:with-gensyms (token)
    `(defnocfun ,(alexandria:format-symbol t "~:@(parse-~a~)" name) ()
       (with-no-errors
           (let ((,token (peek-token *file*)))
             (if (,validator ,token)
                 (progn
                   (next-token *file*)
                   (let ((the-string (do ((the-char (char@1+) (char@1+))
                                          (res ""))
                                         ((string= the-char +rule-stopper+) res)
                                       (setf res (concatenate 'string res the-char)))))
                     (setf ,var the-string)))
                 (push (format nil "Error: looking for ~a ~a found instead." ',name ,token)
                       *parsing-errors*)))))))

(gen-parse-key-string-value-entry (texture-raw *current-texture* is-texture-file-label-p))

(defun parse-texture ()
  (parse-texture-raw)
  (let ((path (string-trim *blank-space* *current-texture*)))
    (if (cl-ppcre:scan fs:+file-path-regex+ path)
        (setf *current-texture* (res:get-resource-file path +trees-resource+))
        ;;(filesystem-utils:sys-datadir-root-path path))
        (push-errors (format nil "Error: invalid texture path: ~a." *current-texture*)))))

(defnocfun parse-color-def ()
  (with-no-errors
    (let ((token (peek-token *file*)))
      (cond
        ((is-color-definition-label-p token)
         (next-token *file*)
         (parse-color-def))
        ((is-float-p token)
         (let ((color
                (loop for rgba from 0 below 4 collect ;; for any color tuple
                     (progn
                       (with-no-errors
                         (let ((parsed-color (next-token *file*)))
                           (if (is-float-p parsed-color)
                               (parse-number->desired parsed-color)
                               (progn
                                 (setf *has-errors* t)
                                 (push (format nil "Error: color definition ended prematurely.")
                                       *parsing-errors*)))))))))
           (push color *palette*)
           (with-no-errors
             (let ((separator (peek-token *file*)))
               (cond
                 ((is-colors-separator-p separator)
                  (next-token *file*)
                  (parse-color-def))
                 ((is-rule-stopper-p separator)
                  (setf *palette* (reverse *palette*))
                  (setf *current-color* (first *palette*))
                  (next-token *file*)))))))))))


(defnocfun parse-tropism ()
  (with-no-errors
    (let ((token (peek-token *file*)))
      (cond
        ((is-tropism-label-p token)
         (next-token *file*)
         (parse-tropism))
        ((is-float-p token)
         (let ((tropism-vector
                (loop for rgba from 0 below 3 collect ;; for any component
                     (progn
                       (with-no-errors
                         (let ((parsed-component (next-token *file*)))
                           (if (is-float-p parsed-component)
                               (parse-number->desired parsed-component)
                               (progn
                                 (setf *has-errors* t)
                                 (push (format nil "Error: tropism vector ended prematurely.")
                                       *parsing-errors*)))))))))
           (setf *current-tropism* (vec (first tropism-vector)
                                        (second tropism-vector)
                                        (third tropism-vector)))
           (with-no-errors
             (let ((separator (peek-token *file*)))
               (if (is-rule-stopper-p separator)
                   (next-token *file*)
                   (progn
                     (setf *has-errors* t)
                     (push (format nil "Error: missing separator '~a' in tropism vector."
                                   +rule-stopper+)
                           *parsing-errors*)))))))))))

(defnocfun parse-axioms ()
  (if (peek-valid-stream)
    (with-no-errors*
      (parse-axiom)
      (parse-axioms))))

(defmacro parse-optional-token ((name validator))
  (alexandria:with-gensyms (token)
    `(defnocfun ,(alexandria:format-symbol t "~:@(parse-optional-~a~)" name) ()
       (let-noerr ((,token (peek-token *file*)))
         (if (,validator ,token)
             (progn
               (next-token *file*)
               ,token)
             nil)))))

(parse-optional-token (inline is-inline-label-p))

(defun expand-rule (name rule &optional (recursive-expand t))
  (if (null rule)
      nil
      (let ((element (first rule)))
        (append
          (cond
            ((and (atom element)
                  (is-rule-names-p element))
             (if (string=  element name) ;; don't expand  itself or if
                 (list name)
                 (let* ((rules-set (gethash element *rules*))
                        (all-rules (and rules-set (rules rules-set)))
                        (expression (and rules-set
                                         all-rules
                                         (expression
                                          (choose-rule rules-set))))
                        (subrule (and rules-set
                                      expression
                                      (not (find-if #'is-conditional-p expression))
                                      expression)))
                   (if (and recursive-expand
                            subrule)
                       (expand-rule element subrule nil)
                       (list element)))))
            ((atom element)
             (list element))
            ((listp element)
             (list (expand-rule name element recursive-expand))))
          (expand-rule name (rest rule) recursive-expand)))))

(defnocfun parse-axiom ()
  (let-noerr* ((inline-p (parse-optional-inline))
               (name (next-token *file*))
               (chance (parse-axiom-chance))
               (equal-sign (next-token *file*)))
    (when (and (is-rule-names-p name)
               (is-equal-p equal-sign))
      (let-noerr* ((expression (parse-expression))
                   (new-axiom (make-instance 'rule
                                             :expression expression
                                             :chance chance)))
        (when (not (gethash name *rules*))
          (setf (gethash name *rules*) (make-instance 'rules-set)))
        (when inline-p
          (setf (expression new-axiom) (expand-rule name expression t)))
        (push new-axiom (rules (gethash name *rules*)))
        (sort-ruleset (gethash name *rules*))))))

(defnocfun parse-axiom-chance ()
  (let-noerr ((chance-label (peek-token *file*)))
    (if (is-chance-param-label-p chance-label)
        (parse-chance-param)
        1.0)))

(defnocfun parse-boolean-expression ()
  (let-noerr ((variable (next-token *file*))
              (operator (next-token *file*))
              (number (next-token *file*)))
    (if (or (is-<-p operator)
            (is->-p operator)
            (is-<=-p operator)
            (is->=-p operator))
        (if (is-float-p number)
            (list variable operator (parse-number->desired number))
            (push-errors (format nil "Error: invalid number (~a) in boolean expression."
                                 number)))
        (push-errors (format nil "Error: invalid operator (~a) in boolean expression."
                                 operator)))))

(defnocfun parse-conditional ()
  (let-noerr ((the-if (next-token *file*))
              (boole-expr (parse-boolean-expression))
              (true-branch (parse-expression))
              (false-branch (parse-expression)))
    (if (is-conditional-p the-if)
        (list the-if boole-expr true-branch false-branch)
        (push-errors (format nil "Error: looking for ~a ~a found instead." +conditional+
                             the-if)))))


(defnocfun parse-expression (&optional (expression ()))
  (let-noerr ((peek (peek-token *file*)))
    (cond
      ((is-pop-p peek)
       (next-token *file*) ;; discard close bracket
       expression)
      ((is-rule-stopper-p peek)
       (next-token *file*) ;; discard stopper
       expression)
      ((is-push-p peek)
       (next-token *file*)
       (let ((sub-expression (parse-expression '())))
         (parse-expression (append expression (list sub-expression)))))
      ((is-conditional-p peek)
       (let ((conditional-expression (parse-conditional)))
         (append expression conditional-expression)))
      (t
       (let ((opcode (reverse (parse-opcode))))
         (setf expression (append expression opcode))
         (parse-expression expression))))))

(defnocfun parse-opcode ()
  (with-no-errors
    (let ((peek (peek-token *file*)))
      (if (and (or (is-opcode-p peek)
                   (is-rule-names-p peek))
               (not (is-rule-stopper-p peek)))
           (progn
             (if (is-opcode-with-argument-p peek)
                 (let-noerr ((opcode (next-token *file*))
                             (argument (parse-number->desired (next-token *file*))))
                   (if argument
                       (list argument opcode)
                       (progn
                         (setf *has-errors* t)
                         (push (format nil "missing argument for ~a." opcode)
                               *parsing-errors*))))
                 (list (next-token *file*))))
           (progn
             (setf *has-errors* t)
             (push (format nil "opcode or rule name expected, found ~a instead." peek)
                   *parsing-errors*))))))

(defmacro gen-push-functions (&rest function-var-stack)
  `(progn
     ,@(loop for i in function-var-stack collect
            `(defun ,(alexandria:format-symbol t "~:@(push-~a~)" (first i)) ()
               (push ,(second i) ,(third i))))))

(defmacro gen-empty-stack-p-functions (&rest stacks-names)
  `(progn
     ,@(loop for i in stacks-names collect
            `(defun ,(alexandria:format-symbol t "~:@(~a-empty-p~)" (string-trim '(#\*) i)) ()
               (null ,i)))))

(gen-empty-stack-p-functions *stack-rotation-angle*
                             *stack-bending-factor*
                             *stack-rotation-axe*
                             *stack-direction*
                             *stack-color*
                             *stack-length*
                             *stack-position*
                             *stack-radius*
                             *stack-s-coord*
                             *stack-t-coord*
                             *stack-s-coord-section*
                             *stack-t-coord-section*
                             *stack-t-coord-section-end*)

(defmacro gen-pop-functions (&rest function-var-stack)
  `(progn
     ,@(loop for i in function-var-stack collect
            `(defun ,(alexandria:format-symbol t "~:@(pop-~a~)" (first i)) ()
               (when (not (,(alexandria:format-symbol t
                                                "~:@(~a-empty-p~)"
                                                (string-trim '(#\*) (third i)))))
                 (setf ,(second i) (pop ,(third i))))))))

(gen-push-functions (rotation-angle *current-rotation-angle* *stack-rotation-angle*)
                    (rotation-axe   *current-rotation-axe*   *stack-rotation-axe*)
                    (bending-factor *current-bending-factor* *stack-bending-factor*)
                    (color *current-color* *stack-color*)
                    (length *current-length* *stack-length*)
                    (position *current-position* *stack-position*)
                    (s-coord *current-s-coord* *stack-s-coord*)
                    (t-coord *current-t-coord* *stack-t-coord*)
                    (s-coord-section *current-s-coord-section* *stack-s-coord-section*)
                    (t-coord-section *current-t-coord-section* *stack-t-coord-section*)
                    (t-coord-section-end *current-t-coord-section-end*
                                         *stack-t-coord-section-end*)
                    (radius *current-radius* *stack-radius*))

(defun push-direction ()
  (push (clone-turtle *current-direction*) *stack-direction*))

(gen-pop-functions (rotation-angle *current-rotation-angle* *stack-rotation-angle*)
                   (rotation-axe   *current-rotation-axe*   *stack-rotation-axe*)
                   (bending-factor *current-bending-factor* *stack-bending-factor*)
                   (direction *current-direction* *stack-direction*)
                   (color *current-color* *stack-color*)
                   (length *current-length* *stack-length*)
                   (position *current-position* *stack-position*)
                   (s-coord *current-s-coord* *stack-s-coord*)
                   (t-coord *current-t-coord* *stack-t-coord*)
                   (s-coord-section *current-s-coord-section* *stack-s-coord-section*)
                   (t-coord-section *current-t-coord-section* *stack-t-coord-section*)
                   (t-coord-section-end *current-t-coord-section-end*
                                        *stack-t-coord-section-end*)
                   (radius *current-radius* *stack-radius*))

(defun push-all ()
 (push-rotation-angle)
 (push-rotation-axe)
 (push-bending-factor)
 (push-direction)
 (push-color)
 (push-length)
 (push-position)
 (push-t-coord)
 (push-s-coord)
 (push-t-coord-section)
 (push-t-coord-section-end)
 (push-s-coord-section)
 (push-radius))

(defun pop-all ()
 (pop-rotation-angle)
 (pop-rotation-axe)
 (pop-bending-factor)
 (pop-direction)
 (pop-color)
 (pop-length)
 (pop-position)
 (pop-t-coord)
 (pop-s-coord)
 (pop-t-coord-section)
 (pop-t-coord-section-end)
 (pop-s-coord-section)
 (pop-radius))

(defun rule-push (what)
  (cond
    ((cl-ppcre:scan what +push-all+)
     (push-all))
    ((cl-ppcre:scan what +push-color+)
     (push-color))
    ((cl-ppcre:scan what +push-direction+)
     (push-direction))
    ((cl-ppcre:scan what +push-rotation-angle+)
     (push-rotation-angle))
    ((cl-ppcre:scan what +push-length+)
     (push-length))
    ((cl-ppcre:scan what +push-position+)
     (push-position))
    ((cl-ppcre:scan what +push-radius+)
     (push-radius))
    ((cl-ppcre:scan what +push-texture-coordinate+)
     (push-s-coord)
     (push-t-coord)
     (push-s-coord-section)
     (push-t-coord-section)
     (push-t-coord-section-end))
    ((cl-ppcre:scan what +push-rotation-axes+)
     (push-rotation-axe))))

(defun pop-from-stacks (what)
    (cond
      ((cl-ppcre:scan what +push-all+)
       (pop-all))
      ((cl-ppcre:scan what +push-color+)
       (pop-color))
      ((cl-ppcre:scan what +push-direction+)
       (pop-direction))
      ((cl-ppcre:scan what +push-rotation-angle+)
       (pop-rotation-angle))
      ((cl-ppcre:scan what +push-length+)
       (pop-length))
      ((cl-ppcre:scan what +push-position+)
       (pop-position))
      ((cl-ppcre:scan what +push-texture-coordinate+)
       (pop-s-coord)
       (pop-t-coord)
       (pop-s-coord-section)
       (pop-t-coord-section)
       (pop-t-coord-section-end))
      ((cl-ppcre:scan what +push-radius+)
       (pop-radius))
      ((cl-ppcre:scan what +push-rotation-axes+)
       (pop-rotation-axe))))

(defparameter *skip-rule* nil)

(defun bool-variable-label->variable (label)
  (cond
    ((string= label +bool-radius-label+)
     *current-radius*)
    ((string= label +bool-length-label+)
     *current-length*)))

(defun bool-operator->function (operator)
  (cond
    ((string= operator +<+)
     #'(lambda (a b) (< a b)))
    ((string= operator +<=+)
     #'(lambda (a b) (<= a b)))
    ((string= operator +>+)
     #'(lambda (a b) (> a b)))
    ((string= operator +>=+)
     #'(lambda (a b) (>= a b)))))

(defun execute-boolean-expression (expression)
  (let ((left-hand (bool-variable-label->variable (first expression)))
        (operator (bool-operator->function (second expression)))
        (right-hand (third expression)))
    (funcall operator left-hand right-hand)))

(defun fire-rule (rule &optional (pushed nil))
  (if (and *skip-rule* (not (null rule)))
      (progn
        (setf *skip-rule* nil)
        (fire-rule (rest rule)))
      (when (< *depth* *max-depth*)
        (when pushed
          (rule-push pushed))
        (let* ((actual-rule (copy-tree rule))
               (expression (first actual-rule)))
          (if (and expression (atom expression))
              (if (is-conditional-p expression)
                  (if (execute-boolean-expression (second actual-rule))
                       (fire-rule (third actual-rule) nil)
                       (fire-rule (fourth actual-rule) nil))
                  (progn
                    (cond
                      ((is-opcode-with-argument-p expression)
                       (pop actual-rule) ;; remove opcode
                       (let ((arg (first actual-rule)))
                         (process-opcode expression arg)))
                      ((is-rule-names-p expression)
                       (let* ((*depth* (1+ *depth*))
                              (chosen-rule (gethash expression *rules*)))
                         (when chosen-rule
                           (fire-rule (expression
                                       (choose-rule (gethash expression *rules*)))))))
                      ((is-delete-branch-p expression)
                       (setf *skip-rule* t))
                      (t
                       (process-opcode expression)))
                    (fire-rule (rest actual-rule) nil)))
              (when (not (null expression))
                (cond
                  ((is-push-opcode-p (first expression))
                   (let ((type-pushed (first expression))
                         (saved-parent (current-node *file*)))
                     (fire-rule (rest expression) type-pushed)
                     (pop-from-stacks type-pushed)
                     (when (push-opcode-makes-physical-branch-p
                            type-pushed)
                       (setf (current-node *file*) saved-parent))
                     (fire-rule (rest actual-rule) nil)))
                  ((is-enter-polygon-mode-p (first expression)) ;; polygon-mode
                   (let ((*polygon-mode* t)
                         (*current-polygon* (make-instance 'triangle-mesh)))
                     (fire-rule (rest expression) nil)
                     (when (> (length (triangles *current-polygon*)) 0)
                       (multiple-value-bind (parent child)
                           (mtree-utils:add-child (current-node *file*) *current-polygon*)
                         (setf (mtree-utils:data child)
                               (make-instance 'section-parameters
                                              :id -1
                                              :radius *current-radius*
                                              :parameters-parent (mtree-utils:data parent)))
                         (average-normals child)
                         (mesh:gen-tangents child))))))))))))

(defmacro gen-process-opcode (&rest var-compare-body)
  `(defun ,(alexandria:format-symbol t "PROCESS-OPCODE") (opcode &optional arg)
     (declare (ignorable arg))
     (declare (optimize (speed 3) (debug 0) (safety 0)))
     (declare (base-string opcode))
     (cond
       ,@(loop for i in var-compare-body collect
              `((,(second i) ,(first i) opcode)
                ,@(rest (rest i)))))))

(defun rotate-direction (angle)
  "angle in degree"
  (rotate-turtle *current-direction* angle :axe *current-rotation-axe*))

(defun draw-section ()
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (let* ((tropism-torsion-vector (cross-product (direction *current-direction*)
                                                *current-tropism*))
         (alpha (d* *current-bending-factor* (rad->deg
                                             (vec-length tropism-torsion-vector)))))
    (rotate-turtle-arbitrary-axe *current-direction* alpha
                                 :axe (if (vec~ tropism-torsion-vector +zero-vec+)
                                          +zero-vec+
                                          (normalize tropism-torsion-vector)))
    (let* ((sign (get-random-float-sign))
           (actual-length (d+ (d* sign
                                  (d* (lcg-next01) *length-randomize-rate*)
                                  *current-length*)
                              *current-length*))
           (new-position (adjust-vec *current-position*
                                     (direction *current-direction*)
                                     actual-length))
           (rotation-mat (if (not (vec~ (cross-product +y-axe+
                                                       (direction *current-direction*))
                                        +zero-vec+))
                             (reorient +y-axe+ (direction *current-direction*))
                             (if (d< (svref (direction *current-direction*) 1) 0.0)
                                 (rotate-around +z-axe+ +pi+)
                                 (identity-matrix))))
           (translation-mat (translate *current-position*)))

      (draw-tree-section actual-length (matrix* translation-mat rotation-mat))
      (setf *current-position* new-position))))

(defun draw-polygon-mode ()
  (let* ((new-position (adjust-vec *current-position*
                                   (direction *current-direction*)
                                   *current-length*)))
    (texel *current-polygon* *current-s-coord* *current-t-coord*)
    (with-epsilon (*current-polygon-magnetic-range*)
       (vertex-v *current-polygon* *current-position* :gen-normal t :gen-triangle t
                 :compact-vertices t :manifoldp nil))
    (setf *current-position* new-position)))

(gen-process-opcode
 ;; set variable value
 (+color+ string=
          (setf *current-color* (select-palette-color arg)))
 (+length+ string=
           (setf *current-length* arg))
 (+angle+ string=
          (setf *current-rotation-angle* arg))
 (+radius+ string=
           (setf *current-radius* arg))
 (+s-coord+ string=
            (setf *current-s-coord* arg))
 (+t-coord+ string=
            (setf *current-t-coord* arg))
 (+s-coord-section+ string=
            (setf *current-s-coord-section* arg))
 (+t-coord-section+ string=
                    (setf *current-t-coord-section* arg))
 (+t-coord-section-end+ string=
                    (setf *current-t-coord-section-end* arg))
 ;; scaling
 (+scale-radius+ string=
                 ;;(misc:dbg "radius scale ~a -> ~a" arg *current-radius*)
                 (setf *current-radius* (d* arg *current-radius*)))
 (+scale-tropism-bending-factor+ string=
                                 (setf *current-bending-factor*
                                       (d* arg *current-bending-factor*)))
 (+scale-length+ string=
                 (setf *current-length* (d* arg *current-length*)))
 (+scale-angle+ string=
                (setf *current-rotation-angle* (d* arg *current-rotation-angle*)))
 (+scale-magnetic-polygon-range+ string=
                                 (setf *current-polygon-magnetic-range*
                                       (d* arg *current-polygon-magnetic-range*)))
 (+scale-section-magnetic-range+ string=
                                 (setf *current-fill-gap-treshold*
                                       (d* arg *current-fill-gap-treshold*)))
 ;; color iteration
 (+next-color+ string=
               (setf *current-color* (next-palette-color)))
 ;; select axys
 (+select-x+ string=
              (setf *current-rotation-axe* :x))
 (+select-y+ string=
             (setf *current-rotation-axe* :y))
 (+select-z+ string=
             (setf *current-rotation-axe* :z))
 ;; incrementing
 (+incr-s-coord+ string=
                 (setf *current-s-coord* (d+ *current-s-coord* arg)))
 (+incr-t-coord+ string=
                 (setf *current-t-coord* (d+ *current-t-coord* arg)))
 (+incr-s-coord-section+ string=
                 (setf *current-s-coord-section* (d+ *current-s-coord-section* arg)))
 (+incr-t-coord-section+ string=
                         (setf *current-t-coord-section* (d+ *current-t-coord-section* arg)))
 (+incr-t-coord-section-end+ string=
                (setf *current-t-coord-section-end* (d+ *current-t-coord-section-end* arg)))
 ;; axys iteration
 (+next-rotation-axe+ string=
                      (setf *current-rotation-axe* (next-rotation-axe *current-rotation-axe*)))
 (+previous-rotation-axe+ string=
                          (setf *current-rotation-axe* (previous-rotation-axe *current-rotation-axe*)))
 ;; rotation
 (+rotate-negative+ string=
                    (let* ((sign (get-random-float-sign))
                           (actual-angle (d+
                                          (d* sign
                                             (d* (the desired-type (lcg-next01))
                                                 *angle-randomize-rate*)
                                             *current-rotation-angle*)
                                          *current-rotation-angle*)))
                      (rotate-direction (d- actual-angle))))
 (+rotate-positive+ cl-ppcre:scan
                    (let* ((sign (get-random-float-sign))
                           (actual-angle (d+
                                          (d* sign
                                             (d* (the desired-type (lcg-next01))
                                                 *angle-randomize-rate*)
                                             *current-rotation-angle*)
                                          *current-rotation-angle*)))
                      (rotate-direction actual-angle)))
 ;; drawing
 (+draw+ cl-ppcre:scan
         (if *polygon-mode*
             (draw-polygon-mode)
             (draw-section)))
 ;; moving
 (+reset-direction+ string=
                    (setf *current-direction* (make-turtle)))

 (+reset-pos+ string=
              (setf *current-position* +zero-vec+))

 (+move+ string=
         (let ((new-position (adjust-vec *current-position*
                                         (direction *current-direction*)
                                         *current-length*)))
           (setf *current-position* new-position))))

;;; rendering

(defmethod prepare-for-rendering ((object lsys-parsed-file))
  (mesh:prepare-for-rendering (mesh object)))

(defmethod render ((object lsys-parsed-file) renderer)
  (mesh:render (mesh object) renderer))

(defparameter *debug-tree-section* nil)

(defun actual-section-divisions () 4)

(let ((id 0))
  (defun draw-tree-section (length transformation)
    (let ((section-mesh (parallelepiped (dsqrt (d* 2.0 (expt *current-radius* 2.0)))
                                        length
                                        :start-s-texture *current-s-coord-section*
                                        :start-t-texture *current-t-coord-section*
                                        :end-t-texture *current-t-coord-section-end*
                                        :max-s-texture (d+ *current-s-coord-section*
                                                           +section-s-coord-offset+)
                                        :wrapper-transformation transformation
                                        :manifold t)))
      (multiple-value-bind (parent child)
          (mtree-utils:add-child (current-node *file*) section-mesh)
        (setf (mtree-utils:data child)
              (make-instance 'section-parameters
                             :id -1
                             :radius *current-radius*
                             :parameters-parent (mtree-utils:data parent)))
        (average-normals child)
        (gen-tangents child)
        (when (and parent
                   (mtree-utils:data parent))
          (let* ((size        *current-radius*)
                 (size-parent (radius (mtree-utils:data parent)))
                 (treshold-sections (d* (max size size-parent)
                                        *current-fill-gap-treshold*)))
            (fill-gaps-between-meshes child parent treshold-sections)
            (average-normals-if-near-vertices child parent (d* *current-radius* 0.1))))
        (setf (current-node *file*) child))
      (incf id))))

(defparameter *test-subdivision-level* 0)

(defun gen-tree (lsys-file &key (flatten t))
    (let ((actual-file (fs:preprocess lsys-file +trees-resource+))
          (the-tree nil))
      (with-lsys-file (:filename actual-file)
        (parse-system)
        (setf the-tree *file*)
        (if (null *parsing-errors*)
            (let ((chosen-rule (choose-rule (gethash +root-rule-label+ *rules*))))
              (when chosen-rule
                (fire-rule (expression chosen-rule))
                (let ((texture (texture:get-texture *current-texture*)))
                  (setf (texture-object (mesh the-tree)) texture)
                  (setf (texture:use-mipmap (texture-object (mesh the-tree))) t)
                  (when (> *test-subdivision-level* 0)
                    (loop repeat *test-subdivision-level* do
                         (setf (mesh the-tree) (subdivide-mesh (mesh the-tree))))
                    (smooth-mesh (mesh the-tree) :average-normals t :plane-mapping t))
                  (remove-orphaned-vertices (mesh the-tree))
                  (texture:prepare-for-rendering (texture-object (mesh the-tree)))
                  (prepare-for-rendering the-tree)
                  (mtree-utils:top-down-visit (mesh the-tree)
                                              #'(lambda (n)
                                                  (setf (mesh:render-aabb     n) nil
                                                        (mesh:render-tangents n) nil
                                                        (mesh:render-normals  n) nil)))
                  (fs:delete-file-if-exists actual-file)
                  (let ((triangles-count 0))
                    (mtree-utils:top-down-visit
                     (mesh the-tree)
                     #'(lambda (n) (incf triangles-count (length (triangles n)))))
                    #+debug-mode (misc:dbg "tree triangles: ~a" triangles-count))
                  (if flatten
                      (let ((actual-mesh (flatten-mesh (mesh the-tree))))
                        (prepare-for-rendering actual-mesh)
                        actual-mesh)
                      (mesh the-tree)))))
            (progn
              (fs:delete-file-if-exists actual-file)
              nil)))))
