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

(in-package :obj-mesh)

(eval-when (:compile-toplevel :load-toplevel :execute)

  (alexandria:define-constant +obj-comment-line+ "#.*\\n|^\\p{Z}*\\n" :test 'string=)

  (alexandria:define-constant +vertex+ "v" :test 'string=)

  (alexandria:define-constant +normal+ "vn" :test 'string=)

  (alexandria:define-constant +texture+ "vt" :test 'string=)

  (alexandria:define-constant +face+ "f" :test 'string=)

  (alexandria:define-constant +face-separator+ "/" :test 'string=)

  (alexandria:define-constant +unknown-token+ "(\\p{L}|\\p{P}|\\p{N})+\\b" :test 'string=)

  (alexandria:define-constant +vertex-and-//+ (concatenate 'string
                                                           text-utils:+integer-regexp+
                                                           +face-separator+
                                                           +face-separator+)
    :test 'string=)

  (alexandria:define-constant +vertex-and-/+ (concatenate 'string
                                                          text-utils:+integer-regexp+
                                                          +face-separator+)
    :test 'string=))

(alexandria:define-constant +texture-filename+ "texture.tga" :test #'string=)

(defclass obj-parsed-file (parsed-file)
  ((mesh
    :initform (make-instance 'mesh:triangle-mesh)
    :initarg :mesh
    :accessor mesh)))

(defmethod initialize-instance :after ((object obj-parsed-file) &key &allow-other-keys)
  (with-slots (comment-line) object
    (with-accessors ((line-mode line-mode)) object
      (setf comment-line +obj-comment-line+
            line-mode t)
      ;; this is just a tangent placeholder for each vertex, preventing parsing fail
      (tangent (mesh object) 0.0 1.0 0.0))))

(defmethod parse-comment-line ((object obj-parsed-file))
  (with-no-errors
    (multiple-value-bind (line length start)
        (buffered-input-file:get-line *file*)
      (declare (ignore length))
      (if (is-comment-line-p object line)
          (parse-comment-line object)
          (progn
            (buffered-input-file:seek *file* start)
            nil)))))

(define-parser-skeleton obj-mesh obj obj-parsed-file)

(define-tokenizer-simple obj-parsed-file  "-?[0-9]+(\\.[0-9]+([eE]-?[0-9]+)?)?"
                                          "0|[1-9][0-9]+|[1-9]"
                                          "^vn" "^vt" "^v" "^f" "/")

(defmethod next-token ((object obj-parsed-file)
                       &key (hook-to-stringpos t) (return-first-match nil)
                         (predicate-sort-tokens
                          #'(lambda (a b) (> (length (first a)) (length (first b))))))
  (declare (ignore hook-to-stringpos return-first-match predicate-sort-tokens))
  (next-token-simple object))

(define-is-stuff-p string=
    +normal+ +texture+ +vertex+ +face+ +face-separator+)

(define-is-stuff-p cl-ppcre:scan
    text-utils:+float-regexp+ text-utils:+integer-regexp+)

(defnocfun parse-line ()
  (when (peek-valid-stream)
    (let-noerr* ((type (peek-token *file*)))
      (cond
        ((is-vertex-p type)
         (next-token-simple *file*)
         (parse-vertex))
        ((is-normal-p type)
         (next-token-simple *file*)
         (parse-normal))
        ((is-texture-p type)
         (next-token-simple *file*)
         (parse-texture))
        ((is-face-p type)
         (next-token-simple *file*)
         (parse-face))
        (t
         #- debug-mode (next-token-simple *file*)
         #+ debug-mode (misc:dbg "obj mesh: discard ~a ~a"
                                 type (next-token-simple *file*))))
      (parse-line))))

(defmacro gen-parse-vertex-normal ((name error) &body body)
  `(defun ,(alexandria:format-symbol t "~:@(parse-~a~)" name) ()
     (let-noerr* ((x (next-token-simple *file*)) ;;anaphora
                  (y (next-token-simple *file*))
                  (z (next-token-simple *file*)))
       (if (and (is-float-regexp-p x)
                (is-float-regexp-p y)
                (is-float-regexp-p z))
           (progn
             ,@body)
           (push-errors ,error)))))

(gen-parse-vertex-normal (vertex (format nil "Vertex coordinates ~a ~a ~a invalid" x y z))
  (vertex (mesh *file*)
          (desired (parse-number:parse-number x))
          (desired (parse-number:parse-number y))
          (desired (parse-number:parse-number z))
          :manifoldp nil
          :compact-vertices t
          :gen-normal nil
          :gen-triangle nil))

(gen-parse-vertex-normal (normal (format nil "Normal coordinates ~a ~a ~a invalid" x y z))
  (normal (mesh *file*)
          (desired (parse-number:parse-number x))
          (desired (parse-number:parse-number y))
          (desired (parse-number:parse-number z))))

(defun parse-texture ()
  (let-noerr* ((x (next-token-simple *file*))
               (y (next-token-simple *file*)))
    (if (and (is-float-regexp-p x) (is-float-regexp-p y))
        (texel (mesh *file*)
               (desired (parse-number:parse-number x))
               (desired (parse-number:parse-number y)))
        (push-errors (format nil "texture coordinates ~a ~a invalid" x y)))))

(defun correct-index (index)
  (let ((index-num (parse-integer index)))
    (if (plusp index-num)
        (1- index-num)
        index-num)))

;f v/t/n v/t/n v/t/n v/t/n
;f v//n v//n v//n
;f v v v
;f v/t v/t v/t
;f 1/1 2/2 3/3

(defun next-token-shorter ()
  (next-token-simple *file* ))

(defun peek-token-shorter ()
  (peek-token* *file*
              :hook-to-stringpos t
              :predicate-sort-tokens #'(lambda (a b)
                                         (< (length (first a)) (length (first b))))))

(defnocfun parse-face ()
  (when (peek-valid-stream)
    (multiple-value-bind (vertex-index start-vertex-index)
        (parse-index)
      (let-noerr* ((index2 (next-token-shorter))
                   (index3 (next-token-shorter)))
        (cond
          ((is-integer-regexp-p index2) ; v v v...
           (process-vvv (mesh *file*) (parse-vvv start-vertex-index)))
          (t
           (if (is-face-separator-p index2)
               (if (is-face-separator-p index3) ; v//n v//n...
                   (process-v//n (mesh *file*) (parse-v//n start-vertex-index))
                   (if (is-integer-regexp-p index3) ; v/t/n v/t/n or v/t v/t...
                       (if (cl-ppcre:scan "\\p{Z}" (string
                                                    (buffered-input-file:get-char *file*)))
                           (process-v/t (mesh *file*)
                                        (nconc
                                         (list (correct-index vertex-index)
                                               (correct-index index3))
                                         (parse-v/t)))
                           (process-v/t/n (mesh *file*)
                                          (parse-v/t/n start-vertex-index)))
                       (push-errors (format nil
                                          "invalid face definition ~a ~a ~a"
                                          vertex-index index2 index3)))))))))))

(defmacro when-planar-poly-p ((vertices-index) &body body)
  (alexandria:with-gensyms (vertices)
    `(let ((,vertices (loop for i in ,vertices-index collect
                           (find-value-by-index mesh (elt i 0)
                                                :what :vertex))))
       (if (same-plane-p ,vertices)
           (progn
             ,@body)
           (misc:dbg "not same plane? ~a" ,vertices)))))

(defun process-vvv (mesh vertices)
  (let ((triangles (ccw-poly-fannify vertices)))
    (when-planar-poly-p ((misc:split-into-sublist vertices 1))
      (loop for i in triangles do
           (let ((normal (triangle-normal (find-value-by-index mesh (elt i 0)
                                                               :what :vertex)
                                          (find-value-by-index mesh (elt i 1)
                                                               :what :vertex)
                                          (find-value-by-index mesh (elt i 2)
                                                               :what :vertex))))
             (loop repeat 3 do
                  (normal-v mesh normal))
             (triangle mesh
                       :compact-vertices nil
                       :manifoldp nil
                       :v1 (elt i 0) :v2 (elt i 1) :v3 (elt i 2)
                       :t1 0 :t2 0 :t3 0
                       :n1 (- (normals-count mesh) 3)
                       :n2 (- (normals-count mesh) 2)
                       :n3 (- (normals-count mesh) 1)
                       :tan1 0 :tan2 0 :tan3 0))))))

(defun process-v//n (mesh vertices)
  (let ((vert-normal (ccw-poly-fannify (misc:split-into-sublist vertices 2))))
    (when-planar-poly-p ((misc:split-into-sublist vertices 2))
      (loop for i in vert-normal do
           (triangle mesh
                     :compact-vertices nil
                     :manifoldp nil
                     :v1 (elt (elt i 0) 0) :v2 (elt (elt i 1) 0) :v3 (elt (elt i 2) 0)
                     :t1 0 :t2 0 :t3 0
                     :n1 (elt (elt i 0) 1) :n2 (elt (elt i 1) 1) :n3 (elt (elt i 2) 1)
                     :tan1 0 :tan2 0 :tan3 0)))))

(defun process-v/t (mesh vertices)
  (let ((indices (misc:split-into-sublist vertices 2)))
    (when-planar-poly-p (indices)
      (let ((vert-tex (ccw-poly-fannify indices)))
        (loop for i in vert-tex do
             (let ((normal (triangle-normal (find-value-by-index mesh (elt (elt i 0) 0)
                                                                 :what :vertex)
                                            (find-value-by-index mesh (elt (elt i 1) 0)
                                                                 :what :vertex)
                                            (find-value-by-index mesh (elt (elt i 2) 0)
                                                                 :what :vertex))))
               (loop repeat 3 do
                    (normal-v mesh normal))
               (triangle mesh
                         :compact-vertices nil
                         :manifoldp nil
                         :v1 (elt (elt i 0) 0) :v2 (elt (elt i 1) 0) :v3 (elt (elt i 2) 0)
                         :t1 (elt (elt i 0) 1) :t2 (elt (elt i 1) 1) :t3 (elt (elt i 2) 1)
                         :n1 (- (normals-count mesh) 3)
                         :n2 (- (normals-count mesh) 2)
                         :n3 (- (normals-count mesh) 1)
                         :tan1 0 :tan2 0 :tan3 0)))))))

(defun process-v/t/n (mesh vertices)
  (let ((vert-tex-normal (ccw-poly-fannify (misc:split-into-sublist vertices 3))))
    (when-planar-poly-p ((misc:split-into-sublist vertices 3))
      (loop for i in vert-tex-normal do
           (triangle mesh
                     :compact-vertices nil
                     :manifoldp nil
                     :v1 (elt (elt i 0) 0) :v2 (elt (elt i 1) 0) :v3 (elt (elt i 2) 0)
                     :t1 (elt (elt i 0) 1) :t2 (elt (elt i 1) 1) :t3 (elt (elt i 2) 1)
                     :n1 (elt (elt i 0) 2) :n2 (elt (elt i 1) 2) :n3 (elt (elt i 2) 2)
                     :tan1 0 :tan2 0 :tan3 0)))))

(defmacro with-rollup ((start-index) &body body)
  `(progn
     (when ,start-index
       (buffered-input-file:seek *file* ,start-index))
     (when (peek-valid-stream)
       ,@body)))

(defnocfun parse-v/t ()
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (when (peek-valid-stream)
    (let-noerr ((index (peek-token-shorter)))
      (if (is-integer-regexp-p index)
          (let ((vertex-index (parse-index))
                (separator (next-token-shorter)))
            (if (is-face-separator-p separator)
                (nconc  (list (correct-index vertex-index)
                              (correct-index (parse-index)))
                        (parse-v/t))
                (push-errors (format nil "missing separator ~a, ~a found instead"
                                     +face-separator+
                                     separator))))
          nil))))

(defnocfun parse-v/t/n (start-index)
  (with-rollup (start-index)
    (if (and (peek-valid-stream)
             (is-integer-regexp-p (peek-token-shorter)))
        (let-noerr ((vertex (parse-index))
                    (separator1 (parse-separator*))
                    (texture (parse-index))
                    (separator2 (parse-separator*))
                    (normal (parse-index)))
          (if (and separator1 separator2)
              (nconc (list (correct-index vertex) (correct-index texture)
                           (correct-index normal))
                     (parse-v/t/n nil))))
          nil)))

(defnocfun parse-vvv (start-index)
  (with-rollup (start-index)
    (let-noerr ((index (peek-token-shorter)))
      (if (is-integer-regexp-p index)
          (nconc (list (correct-index (parse-index)))
                 (parse-vvv nil))
          nil))))

(defnocfun parse-v//n (start-index)
  (with-rollup (start-index)
    (if (and (peek-valid-stream)
             (is-integer-regexp-p (peek-token-shorter)))
        (let-noerr ((vertex (parse-index))
                    (separator1 (parse-separator*))
                    (separator2 (parse-separator*))
                    (normal (parse-index)))
          (if (and separator1 separator2)
              (nconc (list (correct-index vertex)
                           (correct-index normal))
                     (parse-v//n nil))))
          nil)))

(defun parse-separator ()
  (let-noerr* ((separator (next-token-shorter)))
    (if (is-face-separator-p separator)
        separator
        (push-errors (format nil "Expected ~a , ~a found instead" +face-separator+
                             separator)))))

(defun parse-separator* ()
  (let-noerr* ((separator (next-token-shorter)))
    (if (is-face-separator-p separator)
        separator
        nil)))

(defun parse-index ()
  (with-no-errors
    (multiple-value-bind (index start-index)
        (next-token-shorter)
      (if (is-integer-regexp-p index)
          (values index start-index)
          (push-errors (format nil "Expected integer, ~a found instead" index))))))

(defun parse-integer-face-separator ()
  (let-noerr* ((index (next-token-shorter))
               (separator (next-token-shorter)))
    (if (and (is-integer-regexp-p index)
             (is-face-separator-p separator))
        (if (plusp (parse-integer index))
            (1- (parse-integer index))
            (parse-integer index))
        (push-errors (format nil "Expected integer and ~a, ~a, ~a found instead"
                    +face-separator+ index separator)))))

(define-condition invalid-model (conditions:text-error) ())

(defun load (filename &key
                        (texture-path (filesystem-utils:cat-parent-dir
                                          (filesystem-utils:parent-dir-path filename)
                                          +texture-filename+))
                        (average-normals nil))
  (with-obj-file (:filename filename)
    (parse-line)
    (when average-normals
      (average-normals (mesh *file*)))
    (if (not *parsing-errors*)
        (multiple-value-bind (texture errors)
            (texture:get-texture texture-path)
          (if (not errors)
              (progn
                (setf (mesh:texture-object (mesh *file*)) texture)
                (prepare-for-rendering (mesh *file*))
                (texture:prepare-for-rendering (mesh:texture-object (mesh *file*))))
              (error 'conditions:invalid-texture
                     :text (format nil
                                   "Can not load texture ~a for model ~a"
                                   filename
                                   texture-path)))
          (values (mesh *file*) *parsing-errors*))
        (error 'invalid-model :text (format nil "Can not load model ~a" filename)))))
