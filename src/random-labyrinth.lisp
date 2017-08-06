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

(in-package :random-labyrinth)

(alexandria:define-constant +white+                           '(255 255 255) :test 'equalp)

(alexandria:define-constant +black+                           '(0 0 0)       :test 'equalp)

(alexandria:define-constant +seed-color+                      '(255 0 255)   :test 'equalp)

(alexandria:define-constant +border-color+                    '(0 0 255)     :test 'equalp)

(alexandria:define-constant +door-color-n+                    '(0 255 0)     :test 'equalp)

(alexandria:define-constant +door-color-s+                    '(1 255 0)     :test 'equalp)

(alexandria:define-constant +door-color-e+                    '(2 255 0)     :test 'equalp)

(alexandria:define-constant +door-color-w+                    '(3 255 0)     :test 'equalp)

(alexandria:define-constant +window-color-n+                  '(0 255 255)   :test 'equalp)

(alexandria:define-constant +window-color-s+                  '(1 255 255)   :test 'equalp)

(alexandria:define-constant +window-color-e+                  '(2 255 255)   :test 'equalp)

(alexandria:define-constant +window-color-w+                  '(3 255 255)   :test 'equalp)

(alexandria:define-constant +furniture-fountain-color+        '(226 145 145) :test 'equalp)

(alexandria:define-constant +furniture-pillar-color+          '(153 221 146) :test 'equalp)

(alexandria:define-constant +furniture-table-color+           '(147 216 185) :test 'equalp)

(alexandria:define-constant +furniture-chair-color+           '(148 196 211) :test 'equalp)

(alexandria:define-constant +furniture-walkable-color+        '(148 154 206) :test 'equalp)

(alexandria:define-constant +furniture-wall-decoration-color+ '(255 165 96)  :test 'equalp)

(alexandria:define-constant +furniture-other-color+           '(255 0 255)   :test 'equalp)

(alexandria:define-constant +max-generation+                  10000          :test '=)

(alexandria:define-constant +max-rand+                        10000          :test '=)

(alexandria:define-constant +min-room-size+                   1              :test '=)

(defmacro gen-invalicable-p (&rest colors)
  (let ((fn-name (misc:format-fn-symbol t "invalicablep")))
    `(defun ,fn-name (c)
       (or ,@(loop for i in colors collect `(equalp c ,i))))))

(gen-invalicable-p +border-color+
                   +door-color-n+
                   +door-color-s+
                   +door-color-e+
                   +door-color-w+
                   +window-color-n+
                   +window-color-s+
                   +window-color-e+
                   +window-color-w+
                   +furniture-fountain-color+
                   +furniture-table-color+
                   +furniture-chair-color+
                   +furniture-pillar-color+
                   +furniture-other-color+)

(defun seed-color ()
  +seed-color+)

(defun floor-color ()
  +white+)

(defun wallp (c)
  (equalp c +border-color+))

(defun furniturep (c)
  (or (equalp +furniture-fountain-color+        c)
      (equalp +furniture-table-color+           c)
      (equalp +furniture-chair-color+           c)
      (equalp +furniture-pillar-color+          c)
      (equalp +furniture-walkable-color+        c)
      (equalp +furniture-wall-decoration-color+ c)))

(defmacro gen-type-furniture-predicate (name)
  (let ((fn-name (misc:format-fn-symbol t "furniture-~a-p" name)))
    `(defun ,fn-name (c)
       (equalp c ,(alexandria:format-symbol t "~:@(+furniture-~a-color+~)" name)))))

(gen-type-furniture-predicate fountain)

(gen-type-furniture-predicate table)

(gen-type-furniture-predicate chair)

(gen-type-furniture-predicate walkable)

(gen-type-furniture-predicate wall-decoration)

(gen-type-furniture-predicate pillar)

(gen-type-furniture-predicate other)

(defmacro gen-door-side-p (colors sides)
  `(progn
     ,@(loop
          for c in colors
          for s in sides collect
            (let ((fn-name     (alexandria:format-symbol t "~:@(door-~a-p~)" s))
                  (const-name  (alexandria:format-symbol t "~:@(+door-color-~a+~)" s)))
              `(defun ,fn-name (c)
                 (equalp ,const-name c))))))

(gen-door-side-p (+door-color-n+ +door-color-s+ +door-color-e+ +door-color-w+)
                 (n s e w))

(defmacro gen-stuff-p (name &rest colors)
  (let ((fn-name (alexandria:format-symbol t "~:@(~ap~)" name)))
    `(defun ,fn-name (c)
       (or ,@(loop for i in colors collect `(equalp c ,i))))))

(gen-stuff-p door +door-color-n+ +door-color-s+ +door-color-e+ +door-color-w+)

(gen-stuff-p window +window-color-n+ +window-color-s+ +window-color-e+ +window-color-w+)

(defun rand01 ()
  (num:lcg-next01))

(defun safe-random (size)
  (if (<= size 1)
      0
      (+ +min-room-size+ (num:lcg-next-upto (1- size)))))

(defparameter *queue* '())

(defparameter *bfs-visited* '())

(defparameter *generation-count* 0)

(defparameter *ids* 0)

(defun pop-queue ()
  (let ((l (car (last *queue*))))
    (setf *queue* (subseq *queue* 0 (1- (length *queue*))))
    l))

(defun length-queue ()
  (length *queue*))

(defun empty-queue-p ()
  (= 0 (length *queue*)))

(defclass generator-par ()
  ((max-door-num
    :initarg :max-door-num
    :initform 1
    :accessor max-door-num)
   (max-windows-num
    :initarg :max-windows-num
    :initform 5
    :accessor max-windows-num)
   (max-furnitures-num-fn
    :initarg :max-furnitures-num-fn
    :initform #'(lambda (room) (declare (ignore room)) 0)
    :accessor max-furnitures-num-fn)
   (random-fun
    :initarg :random-fun
    :initform #'safe-random
    :accessor random-fun)
   (sigmaw
    :initarg :sigmaw
    :initform 1
    :accessor sigmaw)
   (sigmah
    :initarg :sigmah
    :initform 1
    :accessor sigmah)
   (max-occupied-rate
    :initarg :max-occupied-rate
    :initform 0.99
    :accessor max-occupied-rate)
   (color-scheme
    :initarg :color-scheme
    :initform :grow
    :accessor color-scheme)))

(defmethod marshal:class-persistant-slots ((object generator-par))
  (append '(max-door-num
            max-windows-num
            ;;max-furnitures-num-fn
            ;;random-fun no we can not dump function...
            sigmaw
            sigmah
            max-occupied-rate
            color-scheme)
          (call-next-method)))

(defun aabb-chained-p (aabb1 aabb2)
  (or
   (inside-iaabb2-p aabb1 (elt aabb2 0) (elt aabb2 1))
   (inside-iaabb2-p aabb1 (elt aabb2 2) (elt aabb2 1))
   (inside-iaabb2-p aabb1 (elt aabb2 2) (elt aabb2 3))
   (inside-iaabb2-p aabb1 (elt aabb2 0) (elt aabb2 3))
   (inside-iaabb2-p aabb2 (elt aabb1 0) (elt aabb1 1))
   (inside-iaabb2-p aabb2 (elt aabb1 2) (elt aabb1 1))
   (inside-iaabb2-p aabb2 (elt aabb1 2) (elt aabb1 3))
   (inside-iaabb2-p aabb2 (elt aabb1 0) (elt aabb1 3))))

(defclass lab-room ()
  ((gen-params
    :initarg :gen-params
    :initform (make-instance 'generator-par)
    :reader gen-params)
   (id
    :initarg :id
    :initform *ids*
    :accessor id)
   (parent
    :initarg :parent
    :initform nil
    :accessor parent)
   (x
    :initarg :x
    :initform 0
    :accessor x)
   (y
    :initarg :y
    :initform 0
    :accessor y)
   (width
    :initarg :w
    :initform 0
    :accessor w)
   (height
    :initarg :h
    :initform 0
    :accessor h)
   (color
    :initarg :color
    :initform '(1 1 1)
    :accessor color)
   (doors
    :initarg :doors
    :initform '()
    :accessor doors
    :documentation "'(x y side in [:n :s :e :w] state in [:outside :free :bound])")
   (windows
    :initarg :windows
    :initform '()
    :accessor windows)
   (furnitures
    :initarg :furnitures
    :initform '()
    :accessor furnitures
    :documentation "'(x y)")
   (children
    :initarg :children
    :initform '()
    :accessor children)
   (pixel-scale
    :initarg :pixel-scale
    :initform 1
    :accessor pixel-scale)
   (shared-matrix
    :initarg :shared-matrix
    :initform nil
    :reader shared-matrix)
   (furniture-random-starting-pos-cache
    :initform '()
    :initarg :furniture-random-starting-pos-cache
    :accessor furniture-random-starting-pos-cache)))

(defmethod marshal:class-persistant-slots ((object lab-room))
  (append '(gen-params
            id
            parent
            x
            y
            width
            height
            color
            doors
            windows
            furnitures
            children
            pixel-scale
            shared-matrix)
          (call-next-method)))

(defmacro get-param-gen (object name)
  `(,name (gen-params ,object)))

(defmethod print-object ((object lab-room) stream)
  (let ((*print-circle* t))
    (print-unreadable-object (object stream :type t :identity t)
      (format stream "id ~a parent ~a~%x ~a~%y ~a~%w ~a~%h ~a~%doors ~a~% win ~a~%furnitures ~acolor ~a~%"
              (id object)
              (if (parent object) (id (parent object)) nil)
              (x object)
              (y object)
              (w object)
              (h object)
              (doors object)
              (windows object)
              (furnitures object)
              (color object))
      (format stream "children~%********~%~{~a~%~}**********~%" (mapcar #'id (children object))))))

(defmethod initialize-instance :after ((object lab-room) &key &allow-other-keys)
  (with-accessors ((x x) (y y) (w w) (h h) (shared-matrix shared-matrix)) object
    (when shared-matrix
      (setf x (/ (map-w object) 2))
      (setf y (/ (map-h object) 2))
      (setf w 2)
      (setf h 2)
      (push object *queue*))))

(defgeneric (setf shared-matrix) (matrix object))

(defgeneric (setf gen-params) (params object))

(defgeneric call-random (object limit))

(defgeneric room->mat (object &key draw-door draw-border draw-door-to-nowhere draw-seed
                              draw-id))
(defgeneric debug-room->mat (object &key draw-door draw-border draw-door-to-nowhere draw-seed
                              draw-id))

(defgeneric doors->mat (object &key draw-door-to-nowhere))

(defgeneric door->mat (object door &key draw-door-to-nowhere))

(defgeneric debug-doors->mat (object &key draw-door-to-nowhere))

(defgeneric debug-door->mat (object door &key draw-door-to-nowhere))

(defgeneric windows->mat (object))

(defgeneric window->mat (object win))

(defgeneric furnitures->mat (object))

(defgeneric furniture->mat (object win))

(defgeneric clear-mat (object))

(defgeneric clean-and-redraw-mat (object))

(defgeneric room->dot (object))

(defgeneric find-node (object id))

(defgeneric rotate (object angle))

(defgeneric fit-matrix-to-rooms (object))

(defgeneric scale (object scale-factor))

(defgeneric fill-scale-gap (object))

(defgeneric scale-doors (object scale-factor))

(defgeneric substitute-door (object old new))

(defgeneric delete-door (object door))

(defgeneric scale-windows (object scale-factor))

(defgeneric scale-furnitures (object scale-factor))

(defgeneric translate (object dx dy))

(defgeneric translate-doors (object dx dy))

(defgeneric translate-windows (object dx dy))

(defgeneric translate-furnitures (object dx dy))

(defgeneric rotate-matrix (object angle))

(defgeneric rotate-doors (object angle))

(defgeneric extend-x (object))

(defgeneric extend-y (object))

(defgeneric map-w (object))

(defgeneric map-h (object))

(defgeneric size-limit (object &optional x y))

(defgeneric add-doors (object))

(defgeneric add-windows (object &optional max-number max-recursion))

(defgeneric init-furniture-random-starting-pos-cache (object))

(defgeneric add-furnitures (object &optional max-number max-recursion))

(defgeneric add-door-size (object maxnum &key side))

(defgeneric clean-unused-doors (object &key purge-blind-doors))

(defgeneric clean-furniture-near-door (object))

(defgeneric corner-element-p (object element element-type))

(defgeneric clean-border-doors (object))

(defgeneric corner-position-p (object x y))

(defgeneric clean-superimposed-windows-doors (object))

(defgeneric setf-doorside (object coords))

(defgeneric set-doorside-offset (object))

(defgeneric door-to-nowhere (object door))

(defgeneric get-root (object))

(defgeneric rootp (object))

(defgeneric my-child-p (object child-id))

(defgeneric remove-link-to-child (object child))

(defgeneric grow-on-south (object door))

(defgeneric aabb (object))

(defgeneric whole-aabb (object))

(defgeneric inside-room (object coord))

(defgeneric aabb-overlap-p (object aabb))

(defgeneric outside-map (object coord))

(defgeneric any-overlap-p (object))

(defgeneric max-color (object))

(defgeneric occupied-rate (object))

(defgeneric dfs (object function &rest args))

(defgeneric bfs (object function &rest args))

(defgeneric dump (object file &key draw-door-to-nowhere draw-seed draw-id))

(defgeneric debug-dump (object file &key draw-door-to-nowhere draw-seed draw-id))

(defgeneric dump-dot (object file))

(defgeneric neighbour-element-p (object x y what))

(defmethod (setf shared-matrix) (matrix (object lab-room))
  (labels ((act-setf (obj)
             (with-slots (shared-matrix) obj
               (setf shared-matrix matrix))))
    (dfs object #'act-setf)))

(defmethod (setf gen-params) (params (object lab-room))
  (labels ((act-setf (obj)
             (with-slots (gen-params) obj
               (setf gen-params params))))

    (dfs object #'act-setf)))

(defmethod call-random ((object lab-room) limit)
  (funcall (random-fun (gen-params object)) limit))

(defmethod map-w ((object lab-room))
  (matrix:width (shared-matrix object)))

(defmethod map-h ((object lab-room))
  (matrix:height (shared-matrix object)))

(defmethod debug-door->mat ((object lab-room) door &key (draw-door-to-nowhere t))
  (with-accessors ((matrix shared-matrix)
                   (color color) (pixel-scale pixel-scale)) object
    (let* ((max-color (max-color (get-root object)))
           (door-color (list (first max-color) 0 0))
           (door-size (if (= 0 (floor (/ pixel-scale 4)))
                          1
                          (floor (/ pixel-scale 4))))
           (side (get-door-direction door))
           (x (get-door-x door))
           (y (get-door-y door))
           (door-w (ecase side
                       (:w
                        (* 4 door-size))
                       (:e
                        (* 4 door-size))
                       ((:n :s)
                        door-size)))
           (door-h (ecase side
                       ((:w :e)
                        door-size)
                       ((:n :s)
                        (* 4 door-size))))
           (door-x (ecase side
                       ((:s :n)
                        x)
                       (:e
                        (- (+ x (/ door-size 2)) (/ door-w 2)))
                       (:w
                        (- x (/ door-w 2)))))
           (door-y (ecase side
                     (:n
                      (- y (* 2 door-size)))
                     (:s
                      (- (+ y (/ door-size 2)) (/ door-h 2)))
                     ((:w :e)
                      (- y door-size)))))
      (when (or draw-door-to-nowhere
                (not (door-to-nowhere object door)))
        (matrix:matrix-rect matrix
                            (floor door-x)
                            (floor door-y)
                            door-w
                            door-h
                            (if (door-to-nowhere object door)
                                '(1 0 1)
                                door-color))))))

(defmacro direction->color (element key)
  `(ecase ,key
     (:n ,(alexandria:format-symbol t "~:@(+~a-color-n+~)" element))
     (:s ,(alexandria:format-symbol t "~:@(+~a-color-s+~)" element))
     (:e ,(alexandria:format-symbol t "~:@(+~a-color-e+~)" element))
     (:w ,(alexandria:format-symbol t "~:@(+~a-color-w+~)" element))))

(defmethod door->mat ((object lab-room) door &key (draw-door-to-nowhere t))
  (with-accessors ((matrix shared-matrix)
                   (color color) (pixel-scale pixel-scale)) object
    (let* ((side (get-door-direction door))
           (x (round (get-door-x door)))
           (y (round (get-door-y door)))
           (door-w 1)
           (door-h 1)
           (door-color (direction->color door side)))
      (when (or draw-door-to-nowhere
                (not (door-to-nowhere object door)))
        (matrix:matrix-rect matrix x y door-w door-h door-color)))))

(defmethod debug-doors->mat ((object lab-room) &key (draw-door-to-nowhere t))
  (with-accessors ((x x) (y y) (w w) (h h) (matrix shared-matrix)
                   (color color) (pixel-scale pixel-scale)
                   (children children)) object
    (dfs (get-root object)
         #'(lambda (obj)
             (mapc #'(lambda (door) (debug-door->mat obj door
                                               :draw-door-to-nowhere draw-door-to-nowhere))
                   (doors obj))))))

(defmethod doors->mat ((object lab-room) &key (draw-door-to-nowhere t))
  (with-accessors ((x x) (y y) (w w) (h h) (matrix shared-matrix)
                   (color color) (pixel-scale pixel-scale)
                   (children children)) object
    (dfs (get-root object)
         #'(lambda (obj)
             (mapc #'(lambda (door)
                       (door->mat obj door
                                  :draw-door-to-nowhere draw-door-to-nowhere))
                   (doors obj))))))

(defmethod windows->mat ((object lab-room))
  (with-accessors ((x x) (y y) (w w) (h h) (matrix shared-matrix)
                   (color color) (pixel-scale pixel-scale)
                   (children children)) object
    (dfs (get-root object)
         #'(lambda (obj)
             (mapc #'(lambda (win) (window->mat obj win))
                   (windows obj))))))

(defmethod window->mat ((object lab-room) win)
  (with-accessors ((matrix shared-matrix)) object
    (let* ((x (elt win 0))
           (y (elt win 1))
           (w 1)
           (h 1)
           (window-color (direction->color window (get-window-direction win))))
      (matrix:matrix-rect matrix x y w h window-color))))

(defmethod furnitures->mat ((object lab-room))
  (with-accessors ((x x) (y y) (w w) (h h) (matrix shared-matrix)
                   (color color) (pixel-scale pixel-scale)
                   (children children)) object
    (dfs (get-root object)
         #'(lambda (obj)
             (mapc #'(lambda (f) (furniture->mat obj f))
                   (furnitures obj))))))

(defmethod furniture->mat ((object lab-room) furniture)
  (with-accessors ((matrix shared-matrix)) object
    (let* ((x    (elt furniture 0))
           (y    (elt furniture 1))
           (type (elt furniture 2))
           (w    1)
           (h    1))
      (matrix:matrix-rect matrix x y w h type))))

(defmethod debug-room->mat ((object lab-room)
                      &key (draw-door nil) (draw-door-to-nowhere nil)
                      (draw-border nil)
                      (draw-seed nil)
                      (draw-id nil))
    (labels ((paint (object
                     &key (draw-door nil) (draw-door-to-nowhere nil)
                     (draw-border nil)
                     (draw-seed nil))
               (with-accessors ((id id)
                                (x x) (y y) (w w) (h h) (matrix shared-matrix)
                                (color color) (pixel-scale pixel-scale)
                                (children children)) object

                 (let* ((max-color (max-color (get-root object)))
                        (act-color (if (and draw-seed (rootp object))
                                       (list (first max-color) 0 (first max-color))
                                       color))
                        (border-color (list 0 0 (first max-color)))
                        (border-width (if (= 0 (floor (/ pixel-scale 8)))
                                          1
                                          (floor (/ pixel-scale 8)))))
                   (loop for r from 0 below w do
                        (loop for c from 0 below h do
                             (setf (matrix:matrix-elt matrix (floor (+ y c))
                                                    (floor (+ x r)))
                                   act-color)))
                   (when draw-border
                     (matrix:matrix-rect matrix x y w border-width border-color)
                     (matrix:matrix-rect matrix x (- (+ h y) border-width) w
                                         border-width border-color)
                     (matrix:matrix-rect matrix x y border-width h border-color)
                     (matrix:matrix-rect matrix (- (+ x w) border-width) y
                                         border-width h border-color))
                   (when draw-id
                     (draw-string matrix (round (- (+ x (/ w 2)) (/ +font-w+ 2)))
                                  (round (- (+ y (/ h 2)) (/ +font-h+ 2))) '(0 0 0)
                                  (format nil "~a" id)))
                   (when draw-door
                     (debug-doors->mat object :draw-door-to-nowhere draw-door-to-nowhere))
                   (windows->mat object)
                   (furnitures->mat object)))))
      (dfs object #'(lambda (r) (paint r
                                       :draw-door draw-door
                                       :draw-door-to-nowhere draw-door-to-nowhere
                                       :draw-border draw-border
                                       :draw-seed draw-seed)))
      (shared-matrix object)))

(defmethod room->mat ((object lab-room)
                      &key (draw-door nil) (draw-door-to-nowhere nil)
                      (draw-border nil)
                      (draw-seed nil)
                      (draw-id nil))
    (labels ((paint (object
                     &key (draw-door nil) (draw-door-to-nowhere nil)
                     (draw-border nil)
                     (draw-seed nil))
               (with-accessors ((id id)
                                (x x) (y y) (w w) (h h) (matrix shared-matrix)
                                (children children)) object
                 (let ((act-color (if (and draw-seed (rootp object))
                                      (seed-color)
                                      (floor-color))))
                   (loop for r from 0 to w do
                        (loop for c from 0 to h do
                             (setf (matrix:matrix-elt matrix (floor (+ y c))
                                                      (floor (+ x r)))
                                   act-color)))
                   (when draw-border
                      (matrix:matrix-hline matrix (floor x) (floor y) (floor w)
                                           +border-color+)
                      (matrix:matrix-hline matrix (floor x) (floor (+ h y)) (floor w)
                                           +border-color+)
                      (matrix:matrix-vline matrix (floor x) (floor y) (floor h)
                                           +border-color+)
                      (matrix:matrix-vline matrix (floor (+ x w)) (floor y) (floor h)
                                           +border-color+))
                   (when draw-id
                     (draw-string matrix (round (- (+ x (/ w 2)) (/ +font-w+ 2)))
                                  (round (- (+ y (/ h 2)) (/ +font-h+ 2))) '(0 0 0)
                                  (format nil "~a" id)))
                   (when draw-door
                     (doors->mat object :draw-door-to-nowhere draw-door-to-nowhere))
                   (windows->mat object)
                   (furnitures->mat object)))))
      (dfs object #'(lambda (r) (paint r
                                       :draw-door draw-door
                                       :draw-door-to-nowhere draw-door-to-nowhere
                                       :draw-border draw-border
                                       :draw-seed draw-seed)))
      (shared-matrix object)))

(defmethod room->dot ((object lab-room))
  (let ((graph `(:graph nil)))
    (dfs object
         #'(lambda (r)
             (setf graph (append graph
                                 `((:node ((:id ,(format nil "~a" (id r)))
                                           (:label
                                            ,(format nil "~a" (id r))))))))))
    (dfs object
         #'(lambda (r)
             (mapcar #'(lambda (child)
                         (setf graph (append graph
                                             `((:edge ((:from ,(format nil "~a" (id r)))
                                                       (:to ,(format nil "~a"
                                                                     (id child)))))))))
                     (children r))))
    graph))

(defmethod find-node ((object lab-room) id)
  (dfs object #'(lambda (c) (when (= (id c) id) (return-from find-node c))))
  nil)

(defmethod clear-mat ((object lab-room))
  (with-accessors ((w map-w) (h map-h) (matrix shared-matrix)) object
    (setf matrix (matrix:gen-matrix-frame w h))))

(defmethod clean-and-redraw-mat ((object lab-room))
  (clear-mat object)
  (room->mat object
             :draw-door t
             :draw-door-to-nowhere t
             :draw-border t
             :draw-seed nil
             :draw-id nil))

(defmacro expand-if-possible (root aabb flag direction)
  (alexandria:with-gensyms (new-aabb outside)
    `(let ((,new-aabb (copy-ivec4 ,aabb)))
       ;;(misc:dbg "direction: (~a) ~a~%" ,aabb ,direction)
       (ecase ,direction
           (:n
            (incf (elt ,new-aabb 1) -1))
           (:s
            (incf (elt ,new-aabb 3) 1))
           (:e
            (incf (elt ,new-aabb 2) 1))
           (:w
            (incf (elt ,new-aabb 0) -1)))
       (let ((,outside (or (outside-map ,root (subseq ,new-aabb 0 2))
                           (outside-map ,root (subseq ,new-aabb 2)))))
          (if (and ,flag
                   (not ,outside)
                   (not (aabb-overlap-p ,root ,new-aabb)))
                (setf ,aabb ,new-aabb)
                (setf ,flag nil))))))

(defmethod size-limit ((object lab-room) &optional (x (x object)) (y (y object)))
  (if (and (null (parent object))
           (= 0 (w object)))
      (values x  (- (map-w object) x +min-room-size+)
              y  (- (map-h object) y +min-room-size+))
      (let ((root (get-root object))
            (aabb (ivec4 x y x y))
            (can-expand (list t t t t)))
        (do ()
            ((every #'(lambda (v) (null v)) can-expand) aabb)
          ;; xtop
          (expand-if-possible root aabb (elt can-expand 0) :n)
          ;; y top
          (expand-if-possible root aabb (elt can-expand 1) :s)
          ;; xbottom
          (expand-if-possible root aabb (elt can-expand 2) :e)
          ;; ybottom
          (expand-if-possible root aabb (elt can-expand 3) :w))
        (let ((rect (iaabb2->irect2 aabb)))
          (values (elt rect 0) (elt rect 1)
                  (elt rect 2) (elt rect 3))))))

(defmethod door-to-nowhere ((object lab-room) door)
  (let ((offset (ecase (elt door 2)
                  (:n
                   (list (elt door 0) (1- (elt door 1)) (elt door 2)))
                  (:e
                   (list (1+ (elt door 0)) (elt door 1) (elt door 2)))
                  (:s
                   (list (elt door 0) (1+ (elt door 1)) (elt door 2)))
                  (:w
                   (list (1- (elt door 0)) (elt door 1) (elt door 2))))))
    (not (inside-room (get-root object) (subseq offset 0 2)))))

(defmethod set-doorside-offset ((object lab-room))
  (with-accessors ((doors doors)) object
    (mapcar #'(lambda (d)
                (ecase (elt d 2)
                  (:n
                   (list (elt d 0) (1- (elt d 1)) (elt d 2) (elt d 3)))
                  (:e
                   (list (1+ (elt d 0)) (elt d 1) (elt d 2) (elt d 3)))
                  (:s
                   (list (elt d 0) (1+ (elt d 1)) (elt d 2) (elt d 3)))
                  (:w
                   (list (1- (elt d 0)) (elt d 1) (elt d 2) (elt d 3)))))
            doors)))

(defparameter *dummy-color* 2)

(defmethod grow-on-south ((object lab-room) door)
  (with-accessors ((matrix shared-matrix)) object
    (multiple-value-bind (xtop ytop wrect hrect)
        (size-limit object (elt door 0) (elt door 1))
      (let* ((color-scheme (get-param-gen object color-scheme))
               (child-color
                (if (eq color-scheme :flat)
                    (floor-color)
                    (mapcar #'1+ (color object))))
             (rect-fill (ivec4 xtop ytop wrect hrect))
             (aabb-fill (irect2->iaabb2 rect-fill))
             (rand-sub-rect (trasl-irect2
                             (random-sub-irect2
                               rect-fill
                               (get-param-gen object sigmaw)
                               (get-param-gen object sigmah))
                             (elt door 0) (elt door 1)))
             (aabb (irect2->iaabb2 rand-sub-rect))
             (rect rand-sub-rect)
             (exceed (- (+ (elt rect 0) (elt rect 2))
                        (+ (elt rect-fill 0) (elt rect-fill 2)))))
        (when (and (> (round (elt rect-fill 2)) 1)
                   (> (round (elt rect-fill 3) 1)))
          (let ((child (make-instance 'lab-room
                                      :gen-params (gen-params object)
                                      :shared-matrix matrix
                                      :parent object
                                      :color child-color
                                      :id (1+ *ids*))))
            (incf *ids*)
            (when (> exceed 0) ; did go out of the border
              (setf aabb (trasl-iaabb2 aabb (- exceed) 0))
              (setf rect (iaabb2->irect2 aabb)))
            (let* ((offset-rect (- (elt door 0) (+ (elt rect 0) (elt rect 2))))
                   (offset-diff (- (elt aabb 0) (elt aabb-fill 0)))
                   (offset (if (< offset-rect offset-diff)
                               (- (call-random object offset-rect))
                               (- (call-random object offset-diff)))))
              (setf (x child) (+ (elt rect 0) offset)
                    (y child) (elt rect 1)
                    (w child) (elt rect 2)
                    (h child) (elt rect 3)))
            (if (and (> (w child) 1)
                     (> (h child) 1))
                (progn
                  (push child (children object))
                  (push object (children child))
                  (push child *queue*)
                  t)
                nil)))))))

(defmacro with-multiple-rotation ((object rotation) &body body)
  (alexandria:with-gensyms (root)
    (let* ((angle (if (< rotation 0)
                      (/ (+ rotation 360) 90)
                      (/ rotation 90))))
      `(let ((,root (get-root ,object)))
         (declare (ignorable ,root))
         (unwind-protect
              (progn
                ,@(loop for i from 1 upto angle by 1 collect
                       `(progn
                          (rotate ,root :90)
                          (dfs ,root #'(lambda (r) (rotate-doors r :90)))
                          (rotate-matrix ,object :90))) ;; matrix is shared
                ,@body)
           ,@(loop for i from 1 upto angle by 1 collect
                  `(progn
                     (rotate ,root :-90)
                     (dfs ,root #'(lambda (r) (rotate-doors r :-90)))
                     (rotate-matrix ,root :-90))))))))

(defmacro grow-rotate (object side rotation)
  (alexandria:with-gensyms (child res d)
    `(with-multiple-rotation (,object ,rotation)
       (mapc #'(lambda (door)
                 (if (eq ,side (elt door 2))
                     (let ((,d (add-offset-to-door door)))
                       (cond
                         ((outside-map ,object (subseq ,d 0 2))
                          nil) ; do nothing
                         ((inside-room (get-root ,object) (subseq ,d 0 2))
                          (let ((,child (inside-room (get-root ,object) (subseq ,d 0 2))))
                            (if (not (child-has-same-pos-door-p ,d ,child))
                                (progn
                                  (push ,child (children ,object))
                                  (push ,object (children ,child))
                                  (setf (elt ,d 3) :bound))
                                (setf (elt door 3) :delete))))
                         (t
                          (let ((,res (grow-on-south ,object ,d)))
                            (if ,res
                                (progn
                                  (room->mat ,object)
                                  (setf (elt door 3) :bound))
                                (progn
                                  (setf (elt door 3) :delete)))))))))
             (doors ,object)))))

(defun grow-step (&optional (params nil))
  (labels ((bound-doors (object side)
             (setf (doors object)
                   (mapcar #'(lambda(d)
                               ;; (format t "dump door cerco side ~a ~a~%" side d)
                               ;; (format t "trovato ~a va da qualche parte? ~a~%" d
                               ;;              (not (door-to-nowhere object d)))
                               (list (elt d 0) (elt d 1) (elt d 2)
                                     (if (eq side (elt d 2))
                                         (if (door-to-nowhere object d)
                                             :outside
                                             :bound)
                                         (elt d 3))))
                           (clean-doors (doors object)))))
           (clean-doors (doors)
             (remove-if #'(lambda (d) (eq (get-door-state d) :delete)) doors)))
  (incf *generation-count*)
  (when (not (empty-queue-p))
    (let ((object (pop-queue)))
      (when params
        (setf (gen-params (get-root object)) params))
      (when (not (member object *bfs-visited* :test #'eq))
        (when (<= (occupied-rate (get-root object)) (get-param-gen object max-occupied-rate))
          (push object *bfs-visited*)
          (grow-rotate object :s -90) ; west side
          (bound-doors object :w)
          (grow-rotate object :s 0)   ; south side
          (bound-doors object :s)
          (grow-rotate object :s 90)  ; east side
          (bound-doors object :e)
          (grow-rotate object :s 180) ; north
          (bound-doors object :n)
          (mapc #'add-doors (children object))
          (clear-mat (get-root object))
          (room->mat (get-root object)))
        object)))))

(defun grow-step-windows (room)
  (add-windows room))

(defun grow-furnitures (room)
  ;; redraw all with borders because furniture routines need it
  (room->mat (get-root room) :draw-door t :draw-door-to-nowhere t
             :draw-border t :draw-seed nil :draw-id nil)
  (dfs (get-root room) #'add-furnitures))

(defun grow-single-door (&optional (params nil))
  (labels ((bound-doors (object side)
             (setf (doors object)
                   (mapcar #'(lambda(d)
                               (list (elt d 0) (elt d 1) (elt d 2)
                                     (if (or
                                          (door-to-nowhere object d)
                                          (eq side (elt d 2)))
                                         :bound
                                         (elt d 3))))
                           (doors object)))))
  (incf *generation-count*)
  (when (not (empty-queue-p))
    (let ((object (pop-queue)))
      (when params
        (setf (gen-params (get-root object)) params))
      (when (and (not (member object *bfs-visited* :test #'eq))
             (<= (occupied-rate (get-root object)) (get-param-gen object max-occupied-rate)))
        (push object *bfs-visited*)
        (grow-rotate object :s 90)  ; east side
        (bound-doors object :e)
        (mapc #'add-doors (children object))
        (clear-mat (get-root object))
        (room->mat (get-root object)))))))

(defmacro %gen-update-params-cases (name params func room-fn-args &rest cases)
  `(ecase ,name
     ,@(loop for n in cases collect
            `(,(alexandria:make-keyword n)
               (setf (,n ,params) (funcall ,func *generation-count* ,room-fn-args))))
     (:max-furnitures-num-fn
      (setf (max-furnitures-num-fn params)
            #'(lambda (room) (funcall ,func *generation-count* room))))))

(defun update-params (object name func room-fn-args)
  (let ((params (gen-params (get-root object))))
    (%gen-update-params-cases name params func room-fn-args
                              sigmaw sigmah max-door-num max-windows-num)
    (setf (gen-params (get-root object)) params)))

(defun add-offset-to-door (door)
  (ecase (elt door 2)
    (:n
     (list (elt door 0) (1- (elt door 1)) (elt door 2) (elt door 3)))
    (:e
     (list (1+ (elt door 0)) (elt door 1) (elt door 2) (elt door 3)))
    (:s
     (list (elt door 0) (1+ (elt door 1)) (elt door 2) (elt door 3)))
    (:w
     (list (1- (elt door 0)) (elt door 1) (elt door 2) (elt door 3)))))

(defun door-go-to-border-child-p (door child)
  (with-accessors ((x x) (y y) (w w) (h h)) child
    (let ((door-offset (num-utils:round-all (subseq (add-offset-to-door door) 0 2)))
          (rect (num-utils:round-all (iaabb2->irect2 (aabb child)))))
      (or (equalp door-offset (list (elt rect 0) (elt rect 1)))
          (equalp door-offset (list (+ (elt rect 0) (elt rect 2)) (elt rect 1)))
          (equalp door-offset (list (elt rect 0) (+ (elt rect 1) (elt rect 3))))
          (equalp door-offset (list (+ (elt rect 0) (elt rect 2))
                                    (+ (elt rect 1) (elt rect 3))))))))

(defun child-has-same-pos-door-p (door child)
  (let* ((direction (elt door 2))
         (child-door-guessed (cond
                                ((eq direction :n)
                                 (list (elt door 0) (1- (elt door 1)) :s))
                                ((eq direction :s)
                                 (list (elt door 0) (1+ (elt door 1)) :n))
                                ((eq direction :e)
                                 (list (1+ (elt door 0)) (elt door 1) :w))
                                ((eq direction :w)
                                 (list (1- (elt door 0)) (elt door 1) :e)))))
    (find-if #'(lambda (d)
                 (and
                  (eq (elt d 2) (third child-door-guessed))
                  (or
                   (and (= (elt d 0) (elt door 0))
                        (= (elt d 1) (elt door 1)))
                   (and (= (elt d 0) (elt child-door-guessed 0))
                        (= (elt d 1) (elt child-door-guessed 1))))))
             (doors child))))

(defmethod setf-doorside ((object lab-room) coords)
  (let ((actual-doors (remove-if
                       #'(lambda (d)
                           (let ((res nil))
                             (loop named outer for child in (children object) do
                                  (when (child-has-same-pos-door-p d child)
                                    (setf res t)
                                    (return-from outer)))
                             res))
                       coords)))
    (setf (doors object) (remove-duplicates (append (doors object) actual-doors)
                                            :test #'(lambda (a b)
                                                      (and (= (elt a 0) (elt b 0))
                                                           (= (elt a 1) (elt b 1))
                                                           (eq (elt a 2) (elt b 2))))))))

(defmethod add-doors ((object lab-room))
  (with-accessors ((x x) (y y) (w w) (h h) (doors doors)) object
    (when (= (length doors) 0)
      (let ((max-door-num (get-param-gen object max-door-num)))
        (loop for side in '(:n :e :s :w) collect
             (setf-doorside object
                            (add-door-size object max-door-num :side side)))))))

(defmethod neighbour-element-p ((object lab-room) x y what)
  (labels ((find-stuff (coords where)
             (find (num-utils:round-all coords) where
                   :test #'(lambda (a b) (equalp (num-utils:round-all a)
                                                 (num-utils:round-all b))))))
    (let ((res nil))
      (loop for i in (matrix:gen-neighbour-position x y) do
           (dfs (get-root object)
                #'(lambda (obj)
                    (ecase what
                      (:windows
                       (when (find-stuff i (mapcar #'get-window-coordinates (windows obj)))
                         (setf res t)))
                      (:doors
                       (when (find-stuff i (mapcar #'get-door-coordinates (doors obj)))
                         (setf res t)))
                      (:furnitures
                       (when (find-stuff i (mapcar #'get-furniture-coordinates (furnitures obj)))
                         (setf res t)))
                      (:any
                       (when (or
                              (find-stuff i (mapcar #'get-window-coordinates (windows obj)))
                              (find-stuff i (mapcar #'get-door-coordinates (doors obj)))
                              (find-stuff i (mapcar #'get-furniture-coordinates (furnitures obj))))
                         (setf res t)))))))
      res)))

(defmethod add-windows ((object lab-room)
                        &optional
                          (max-number (num:lcg-next-upto (get-param-gen object max-windows-num)))
                          (max-recursion 5000))
  (with-accessors ((x x) (y y) (w w) (h h) (doors doors) (windows windows)) object
    (if (and
             (> max-number 0)
             (> max-recursion 0))
        (let* ((win-y (round (+ y (* (rand01) h))))
               (win-x (if (or (= win-y y)
                              (= win-y (+ y h)))
                          (+ (1+ x) (floor (* (rand01) (1- w))))
                          (+ x (floor (* (num:lcg-next-upto 2) w))))))
          (if (neighbour-element-p object win-x win-y :doors)
              (add-windows object max-number (1- max-recursion))
              (let ((new-window (list win-y win-x)))
                (cond
                  ((= win-y y)
                   (push :n new-window))
                  ((= win-y (+ y h))
                   (push :s new-window))
                  ((= win-x x)
                   (push :w new-window))
                  (t
                   (push :e new-window)))
                (push (reverse new-window) windows)
                (add-windows object (1- max-number) (1- max-recursion))))))))

(defun template-compatible-element-p (matrix-element template-element)
  (cond
    ((eq template-element :wall)
     (wallp   matrix-element))
    ((eq template-element :window)
     (windowp matrix-element))
    ((eq template-element :door)
     (doorp   matrix-element))
    ((eq template-element :empty)
     (equalp matrix-element (floor-color)))
    (t
     (equalp matrix-element (floor-color)))))

(defun pixel->furniture-template (pixel)
  (let ((element (subseq (coerce pixel 'list) 0 3)))
    (cond
      ((wallp element)
       :wall)
      ((windowp element)
       :window)
      ((doorp element)
       :door)
      ((equalp element (floor-color))
       :empty)
      (t
       element))))

(defun template-compatible-p (matrix template x y)
  (matrix:submatrix= matrix template x y
                     :test #'(lambda (a b)
                               (and (= (length a) (length b))
                                    (progn
                                      (loop
                                         for i across a
                                         for j across b do
                                           (if (not (template-compatible-element-p i j))
                                               (return-from template-compatible-p nil)))
                                      t)))))

(defun count-furniture-units (template)
  (truncate (* 0.2 (length (matrix:data template)))))

(defun count-furniture-units-for-sort (template &key (scale-weight 4.0))
  "Give more weight to generic furnitures"
  (- (count-if #'(lambda (a) (not (keywordp a))) (matrix:data template))
     (* scale-weight (count-if #'(lambda (a) (furniture-other-p a)) (matrix:data template)))))

(defun pixmap->template (pixmap)
  (matrix:map-matrix pixmap #'(lambda (a) (pixel->furniture-template a))))

(defun pixmap-file->template (file)
  (pixmap->template (pixmap:slurp-pixmap 'pixmap:tga file)))

(defun all-templates ()
  (let ((all (remove-if-not #'(lambda (a)
                                (filesystem-utils:has-extension (uiop:native-namestring
                                                                 (uiop:native-namestring a))
                                                                "tga"))
                            (res:get-resource-files +default-furniture-templates-dir+))))
    (sort (mapcar #'(lambda (a) (pixmap-file->template (uiop:native-namestring a)))
                  all)
          #'(lambda (a b) (< (count-furniture-units-for-sort a)
                             (count-furniture-units-for-sort b))))))

(defmethod init-furniture-random-starting-pos-cache ((object lab-room))
  (with-accessors ((x x) (y y) (w w) (h h)
                   (furniture-random-starting-pos-cache furniture-random-starting-pos-cache)
                   (shared-matrix shared-matrix)) object
    (when (null furniture-random-starting-pos-cache)
      (setf furniture-random-starting-pos-cache
            (nconc
             (loop for xstart from x to (+ x w) collect
                  (list xstart y))
             (loop for xstart from x to (+ x w) collect
                  (list xstart (+ y h)))
             (loop for ystart from y to (+ y h) collect
                  (list x ystart))
             (loop for ystart from y to (+ y h) collect
                  (list (+ x w) ystart))
             (let ((new-values '()))
               (loop for xstart from (1+ x) below (+ x w) by (floor (/ w 4)) do
                    (loop for ystart from (1+ y) below (+ y h) by (floor (/ h 4)) do
                         (push (list xstart ystart) new-values)))
               new-values))))
    furniture-random-starting-pos-cache))

(defun actual-funiture-count (room)
  (max 1 (funcall (get-param-gen room max-furnitures-num-fn) room)))


(defmethod add-furnitures ((object lab-room) &optional
                                               (max-number (actual-funiture-count object))
                                               (max-recursion 50))
  (with-accessors ((x x) (y y) (w w) (h h) (furnitures furnitures)
                   (shared-matrix shared-matrix) (aabb aabb)) object
    (if (and
         (> max-number 0)
         (> max-recursion 0))
        (let* ((candidates (init-furniture-random-starting-pos-cache object))
               (furn-pos (misc:random-elt candidates))
               (furn-y (elt furn-pos 1))
               (furn-x (elt furn-pos 0)))
          (dolist (template (if (evenp max-recursion)
                                (all-templates)
                                (reverse (all-templates))))
            (when (template-compatible-p shared-matrix template furn-x furn-y)
              (matrix:loop-matrix (template x y)
                (let ((element (matrix:matrix-elt template y x)))
                  (when (not (keywordp element))
                    (push (list (+ furn-x x) (+ furn-y y) element)
                          furnitures))))
              (clean-and-redraw-mat (get-root object))
              (decf max-number (count-furniture-units template))
              (setf furn-pos (misc:random-elt candidates)
                    furn-y (elt furn-pos 1)
                    furn-x (elt furn-pos 0))))
          (add-furnitures object
                          max-number
                          (1- max-recursion))))))

(defun add-door-coord (from to howmany)
  (do ((res '()))
      ((= (length res) howmany) (sort res #'<))
    (let ((new-door (+ from (num:lcg-next-upto (- to from)))))
      (when (not (find new-door res :test #'=))
        (push new-door res)))))

(defun slide-coord-along-border (coord delta)
  (ecase (elt coord 2)
    ((:e :w)
     (list (elt coord 0) (+ delta (elt coord 1))
           (elt coord 2) (elt coord 3)))
    ((:n :s)
     (list (+ delta (elt coord 0)) (elt coord 1)
           (elt coord 2) (elt coord 3)))))

(defmethod add-door-size ((object lab-room) maxnum &key side)
  (labels ((door-rand-num (max)
             (num:lcg-next-upto (1+ max))))
    (with-accessors ((x x) (y y) (w w) (h h) (matrix shared-matrix)) object
      (let ((max-doors-along-x-axe (1- w))
            (max-doors-along-y-axe (1- h)))
      (ecase side
        (:n
         (let* ((act-doornum (door-rand-num (if (>= max-doors-along-x-axe maxnum)
                                                maxnum max-doors-along-x-axe)))
                (coord (add-door-coord (1+ x) (+ x w) act-doornum)))
           (loop for i in coord collect (list i y :n :free))))
        (:s
         (let* ((act-doornum (door-rand-num (if (>= max-doors-along-x-axe maxnum)
                                                maxnum max-doors-along-x-axe)))
                (coord (add-door-coord (1+ x) (+ x w) act-doornum)))
           (loop for i in coord collect (list i (+ y h) :s :free))))
        (:e
         (let* ((act-doornum (door-rand-num (if (>= max-doors-along-y-axe maxnum)
                                                maxnum max-doors-along-y-axe)))
                (coord (add-door-coord (1+ y) (+ y h) act-doornum)))
           (loop for i in coord collect (list (+ x w) i :e :free))))
        (:w
         (let* ((act-doornum (door-rand-num (if (>= max-doors-along-y-axe maxnum)
                                                maxnum max-doors-along-y-axe)))
                (coord (add-door-coord (1+ y) (+ y h) act-doornum)))
           (loop for i in coord collect (list x i :w :free)))))))))

(defmethod clean-unused-doors ((object lab-room) &key (purge-blind-doors nil))
  (dfs object #'(lambda (obj) (setf (doors obj)
                                    (remove-if #'(lambda (d)
                                                   (if purge-blind-doors
                                                       (eq (elt d 3) :free)
                                                       (and
                                                        (not (door-to-nowhere obj d))
                                                        (eq (elt d 3) :free))))
                                               (doors obj))))))

(defmethod clean-furniture-near-door ((object lab-room))
  (dfs object #'(lambda (obj)
                  (setf (furnitures obj)
                        (remove-if #'(lambda (f)
                                       (neighbour-element-p obj (elt f 0) (elt f 1) :doors))
                                   (furnitures obj))))))

(defmethod corner-position-p ((object lab-room) x y)
  (let ((r-x (num-utils:round-all x))
        (r-y (num-utils:round-all y)))
    (cond
      ((and (= r-x (round (x object)))
            (= r-y (round (y object))))
       :nw)
      ((and (= r-x (round (+ (x object) (w object))))
            (= r-y (round (y object))))
       :ne)
      ((and (= r-x (round (+ (x object) (w object))))
            (= r-y (round (+ (y object) (h object)))))
       :se)
      ((and (= r-x (round (x object)))
            (= r-y (round (+ (y object) (h object)))))
       :sw)
      (t
       nil))))

(defmethod corner-element-p ((object lab-room) element element-type)
  (let ((res nil)
        (x (ecase element-type
             (:door
              (get-door-x element))
             (:window
              (get-window-x element))))
        (y (ecase element-type
             (:door
              (get-door-y element))
             (:window
              (get-window-y element)))))
    (dfs object
         #'(lambda (obj)
             (when (corner-position-p obj x y)
               (setf res t))))
    res))

(defmethod clean-border-doors ((object lab-room))
    (dfs object
         #'(lambda (room)
             (loop for c in (children room) do
                  (loop for d in (doors room) do
                       (when (door-go-to-border-child-p d c)
                         (let ((door+1 (slide-coord-along-border d +1))
                               (door-1 (slide-coord-along-border d -1)))
                           (delete-door room d)
                           (if (and
                                (not (door-go-to-border-child-p door+1 c))
                                (not (neighbour-element-p room
                                                          (elt door+1 0)
                                                          (elt door+1 1) :doors))
                                (inside-room room (add-offset-to-door door+1))
                                (= (id (inside-room room (add-offset-to-door door+1)))
                                   (id c))
                                (not (corner-position-p room (elt door+1 0)
                                                        (elt door+1 1))))
                               (push door+1 (doors room))
                               (if (and
                                    (not (door-go-to-border-child-p door-1 c))
                                    (not (neighbour-element-p room (elt door-1 0)
                                                              (elt door-1 1) :doors))
                                    (inside-room room (add-offset-to-door door-1))
                                    (= (id (inside-room room (add-offset-to-door door-1)))
                                       (id c))
                                    (not (corner-position-p room (elt door-1 0)
                                                            (elt door-1 1))))

                                   (push door-1 (doors room))
                                   (progn
                                     (remove-link-to-child room c)
                                     (remove-link-to-child c room)))))))))))

(defmethod clean-superimposed-windows-doors ((object lab-room))
  (dfs object
       #'(lambda (room)
           (loop for c in (children room) do
                (loop for w in (windows room) do
                     (when (neighbour-element-p c (elt w 0) (elt w 1) :doors)
                       (setf (windows room)
                             (remove-if
                              #'(lambda (window)
                                  (equalp (num-utils:round-all (get-door-coordinates window))
                                          (num-utils:round-all (get-door-coordinates w))))
                              (windows room)))))))))

(defmethod get-root ((object lab-room))
  (if (null (parent object))
      object
      (get-root (parent object))))

(defmethod rootp ((object lab-room))
  (null (parent object)))

(defmethod my-child-p ((object lab-room) child-id)
  (find-if #'(lambda (child) (= (id child) child-id))
           (children object)))

(defmethod remove-link-to-child ((object lab-room) (child number))
  (setf (children object)
        (remove-if #'(lambda (c)
                       (if (= (id c) child)
                           (progn
                             ;;(misc:dbg "remove ~a from ~a" (id c) (id object))
                             t)
                           nil))
                   (children object) :count 1)))

(defmethod remove-link-to-child ((object lab-room) (child lab-room))
  (remove-link-to-child object (id child)))

(defmethod aabb ((object lab-room))
  (with-accessors ((x x) (y y) (w w) (h h)) object
    (ivec4 x y
          (if (epsilon= 0 w)
              x
              (+ x w))
          (if (epsilon= 0 h)
              y
              (+ y h)))))

(defmethod whole-aabb ((object lab-room))
  (let ((whole (ivec4 1000000 1000000 -1 -1)))
    (dfs object #'(lambda (obj)
                    (setf whole (union-iaabb2 whole (aabb obj)))))
    whole))

(defmethod inside-room ((object lab-room) coord)
  (labels ((act-inside-room (obj)
             (with-accessors ((x x) (y y)) obj
               (let ((aabb (aabb obj)))
                 (when (inside-iaabb2-p aabb (elt coord 0) (elt coord 1))
                   (return-from inside-room obj))))))
    (dfs object #'act-inside-room)
    nil))

(defmacro with-act-dfs ((name params &rest fun-body) &body body)
  `(labels ((,(alexandria:format-symbol t "~@:(act-~a~)" name) (,@params)
              ,@fun-body))
     ,@body))


(defmethod aabb-overlap-p ((object lab-room) aabb)
  (let ((res nil))
    (labels ((act-aabb-overlap-p (obj)
               (let ((aabb-obj (aabb obj)))
                 (when (aabb-chained-p (num-utils:round-all aabb)
                                       (num-utils:round-all aabb-obj))
                   (setf res t)))))
      (dfs object #'act-aabb-overlap-p)
      res)))

(defmethod any-overlap-p ((object lab-room))
  (let ((aabb (aabb object)))
    (labels ((dfs-overlap ()
                 (dfs (get-root object)
                      #'(lambda (obj aabb)
                          (when (and (not (eq obj object))
                                     (aabb-chained-p (aabb obj) aabb)))
                          t)
                      aabb)))
      (dfs-overlap)
      (mapc #'any-overlap-p (children object)))))

(defmethod dfs ((object lab-room) (function function) &rest args)
  (let ((blacks '()))
    (labels ((actual-dfs (object function &rest args)
                (when (not (member object blacks :test #'eq))
                  (push object blacks)
                  (apply function (append (list object) args))
                  (mapcar #'(lambda (obj)
                              (apply #'actual-dfs obj function args))
                          (children object)))))
      (apply #'actual-dfs object function args))))

(defmethod bfs ((object lab-room) (function function) &rest args)
  (let ((queue '())
        (blacks '()))
    (labels ((pop-queue ()
               (let ((value (alexandria:lastcar queue)))
                 (setf queue (subseq queue 0 (1- (length queue))))
                 value))
             (empty-queue-p ()
               (= 0 (length queue))))
      (push object queue)
      (do ()
          ((empty-queue-p))
        (let ((element (pop-queue)))
          (when (and element
                     (not (member element blacks :test #'eq)))
            (push element blacks)
            (apply function (append (list element) args))
            (loop for c in (children element) do
                 (push c queue))))))))

(defmethod outside-map ((object lab-room) coord)
  (or (< (elt coord 0) 0)
      (>= (elt coord 0) (map-w object))
      (>= (elt coord 1) (map-h object))
      (< (elt coord 1) 0)))

(defmethod rotate ((object lab-room) angle)
  (labels ((act-rotate (obj rot-angle)
             (with-accessors ((x x) (y y) (w w) (h h) (children children)) obj
               (let* ((aabb (irect2->iaabb2 (ivec4 x y (1+ w) (1+ h))))
                      (pivot (list (/ (map-w object) 2) (/ (map-h object) 2)))
                      (act-angle (90deg->rad rot-angle))
                      (rotated (iaabb2->irect2 (rotate-iaabb2 aabb act-angle pivot))))
                 (setf x (elt rotated 0))
                 (setf y (elt rotated 1))
                 (setf w (1- (elt rotated 2)))
                 (setf h (1- (elt rotated 3)))))))
    (dfs object #'(lambda (obj) (act-rotate obj angle)))))

(defmethod scale-doors ((object lab-room) scale-factor)
  (setf (doors object)
        (scale-coords (doors object) scale-factor)))

(defun test-rounded-coord-function ()
  #'(lambda (a b)
      (equalp (num-utils:round-all (if (> (length a) 2) (subseq a 0 2) a))
              (num-utils:round-all (if (> (length b) 2) (subseq b 0 2) b)))))

(defmethod substitute-door ((object lab-room) old new)
  (let ((rest-doors (remove old (doors object) :test (test-rounded-coord-function))))
    (setf (doors object) (append rest-doors (list new)))))

(defmethod delete-door ((object lab-room) door)
  (let ((rest-doors (remove door (doors object) :test (test-rounded-coord-function))))
    (setf (doors object) rest-doors)))

(defun scale-coords (coords scale-factor)
  (mapcar #'(lambda (coord) (append
                             (list (* scale-factor (elt coord 0))
                                   (* scale-factor (elt coord 1)))
                             (subseq coord 2)))
          coords))

(defmethod scale-windows ((object lab-room) scale-factor)
  (setf (windows object) (scale-coords (windows object) scale-factor)))

(defmethod scale-furnitures ((object lab-room) scale-factor)
  (setf (furnitures object) (scale-coords (furnitures object) scale-factor)))

(defmethod fit-matrix-to-rooms ((object lab-room))
  (let* ((aabb (whole-aabb (get-root object)))
         (new-matrix (matrix:gen-matrix-frame (+ 1 (elt aabb 2))
                                              (+ 1 (elt aabb 3)))))
    (setf (shared-matrix (get-root object)) new-matrix)))

(defmethod scale ((object lab-room) scale-factor)
  (labels ((bound-non-corner-door (object door)
             (if (eq (get-door-state door) :outside)
                 (let ((inside-child (inside-room object
                                                  (add-offset-to-door door))))
                   (if inside-child
                       (progn
                         (push inside-child (children object))
                         (push object (children inside-child))
                         (list (get-door-x door)
                               (get-door-y door)
                               (get-door-direction door)
                               :bound))
                       door))
                 door))
           (rearrange-corner-door (object door)
             (if (and
                    (not (corner-element-p object door :door))
                    (not (neighbour-element-p object
                                              (get-door-x door)
                                              (get-door-y door) :doors)))
                 (let ((inside-child (inside-room object
                                                  (add-offset-to-door door))))
                   (if inside-child
                       (progn
                         (push inside-child (children object))
                         (push object (children inside-child))
                         (list (get-door-x door)
                               (get-door-y door)
                             (get-door-direction door)
                             :bound))
                       door))
                 nil)))
    (dfs object
         #'(lambda (obj)
             (setf (x obj) (* (x obj) scale-factor)
                   (y obj) (* (y obj) scale-factor)
                   (w obj) (* (w obj) scale-factor)
                   (h obj) (* (h obj) scale-factor)
                   (pixel-scale obj) scale-factor)
             (scale-doors obj scale-factor)
             (scale-windows obj scale-factor)
             (scale-furnitures obj scale-factor)))
    (fill-scale-gap object)
    (fit-matrix-to-rooms object)
    (dfs object
         #'(lambda (obj)
             (setf (doors obj)
                   (remove-if #'null
                              (mapcar #'(lambda (door)
                                          (if (corner-element-p obj door :door)
                                              (let ((door+1 (slide-coord-along-border door +1))
                                                    (door-1 (slide-coord-along-border door -1)))
                                                (delete-door obj door)
                                                (or (rearrange-corner-door obj door+1)
                                                    (rearrange-corner-door obj door-1)))
                                              (bound-non-corner-door obj door)))
                                    (doors obj))))))))

(defmethod fill-scale-gap ((object lab-room))
  (dfs object
       #'(lambda (obj)
           (extend-x obj)
           (extend-y obj)
           (setf (doors obj)
                 (mapcar #'(lambda (door)
                             (ecase (get-door-direction door)
                               (:s
                                (list (+ (pixel-scale obj) (get-door-x door))
                                      (+ (pixel-scale obj) (get-door-y door))
                                      :s
                                      (get-door-state door)))
                               (:e
                                (list (+ (pixel-scale obj) (get-door-x door))
                                      (+ (pixel-scale obj) (get-door-y door))
                                      :e
                                      (get-door-state door)))
                               (:w
                                (list (get-door-x door)
                                      (+ (pixel-scale obj) (get-door-y door))
                                      :w
                                      (get-door-state door)))

                               (:n
                                (list (+ (pixel-scale obj) (get-door-x door))
                                      (get-door-y door)
                                      :n
                                      (get-door-state door)))))
                         (doors obj)))
           (setf (windows obj)
                 (mapcar #'(lambda (window)
                             (ecase (get-window-direction window)
                               (:s
                                (list (+ (pixel-scale obj) (get-window-x window))
                                      (+ (pixel-scale obj) (get-window-y window))
                                      :s))
                               (:e
                                (list (+ (pixel-scale obj) (get-window-x window))
                                      (+ (pixel-scale obj) (get-window-y window))
                                      :e))
                               (:w
                                (list (get-window-x window)
                                      (+ (pixel-scale obj) (get-window-y window))
                                      :w))
                               (:n
                                (list (+ (pixel-scale obj) (get-window-x window))
                                      (get-window-y window)
                                      :n))))
                         (windows obj))))))

(defmethod translate-doors ((object lab-room) dx dy)
  (setf (doors object) (translate-coords (doors object) dx dy)))

(defun translate-coords (coords dx dy)
  (mapcar #'(lambda (coord)
              (append (list (+ dx (elt coord 0))
                            (+ dy (elt coord 1)))
                      (subseq coord 2)))
          coords))

(defun get-door-coordinates (door)
  (subseq door 0 2))

(defun get-furniture-coordinates (furniture)
  (subseq furniture 0 2))

(defun get-door-x (door)
  (first (get-door-coordinates door)))

(defun get-door-y (door)
  (second (get-door-coordinates door)))

(defun get-door-state (door)
  (elt door 3))

(defun get-door-direction (door)
  (elt door 2))

(defun get-window-coordinates (window)
  (subseq window 0 2))

(defun get-window-x (window)
  (first (get-window-coordinates window)))

(defun get-window-y (window)
  (second (get-window-coordinates window)))

(defun get-window-direction (window)
  (elt window 2))

(defmethod translate-windows ((object lab-room) dx dy)
  (setf (windows object) (translate-coords (windows object) dx dy)))

(defmethod translate-furnitures ((object lab-room) dx dy)
  (setf (furnitures object) (translate-coords (furnitures object) dx dy)))

(defmethod translate ((object lab-room) dx dy)
  (dfs object
       #'(lambda (obj)
           (setf (x obj) (+ (x obj) dx)
                 (y obj) (+ (y obj) dy))
           (translate-doors obj dx dy)
           (translate-windows obj dx dy)
           (translate-furnitures obj dx dy))))

(defmethod rotate-matrix ((object lab-room) angle)
  (with-accessors ((matrix shared-matrix) (children children)) object
    (let ((rot (rotate-matrix* matrix angle)))
      (setf matrix rot))))

(defun rotate-matrix* (mat angle &optional (pivot (list (truncate (/ (matrix:width mat) 2))
                                                        (truncate (/ (matrix:height mat) 2)))))
  (let ((res (matrix:gen-matrix-frame (matrix:height mat) (matrix:width mat)))
        (aabb (ivec4 1000 1000 -1 -1))
        (vals nil)
        (act-angle (90deg->rad angle)))
    (do ((i 0 (1+ i)))
        ((not (< i (matrix:height  mat))))
      (do ((j 0 (1+ j)))
          ((not (< j (matrix:width mat))))
        (let* ((rotated (mapcar #'round
                                (2d-vector-rotate (list (- j (elt pivot 0))
                                                        (- i (elt pivot 1)))
                                                  act-angle)))
               (retlas (list (+ (elt pivot 0) (elt rotated 0))
                             (+ (elt pivot 1) (elt rotated 1)))))
          (push (list retlas (matrix:matrix-elt mat j i)) vals)
          (setf aabb (expand-iaabb2 aabb retlas)))))

    ;(format t "vals ~a% aabb ~a~%" vals aabb)
    (mapcar #'(lambda (v)
                (setf (matrix:matrix-elt res
                                       (floor (- (second (elt v 0)) (elt aabb 1)))
                                       (floor (- (first (elt v 0)) (elt aabb 0))))
                      (elt v 1)))
            vals)
    res))

(alexandria:define-constant +door-clockwise+ '(:n :e :s :w) :test 'equalp)

(defmethod rotate-doors ((object lab-room) angle)
  (with-accessors ((doors doors) (x x) (y y) (w map-w) (h map-h)) object
    (let ((coord-angle (90deg->rad angle))
          (door-shift-fun  (ecase angle
                             (:90
                              #'(lambda (d)
                                  (nth
                                   (mod (1+ (position d +door-clockwise+))
                                        (length +door-clockwise+))
                                   +door-clockwise+)))
                             (:-90
                              #'(lambda (d)
                                  (let ((pos (1- (position d +door-clockwise+))))
                                    (if (< pos 0)
                                        (car (last +door-clockwise+))
                                        (nth pos +door-clockwise+)))))))
          (pivot (list (/ w 2) (/ h 2))))
      (setf doors
            (mapcar #'(lambda (d)
                        (let* ((aabb-door (ivec4 (elt d 0)
                                                 (elt d 1)
                                                 (1+ (elt d 0))
                                                 (1+ (elt d 1))))
                               (aabb-rot (rotate-iaabb2 aabb-door coord-angle pivot))
                               (res (list (elt aabb-rot 0)
                                          (elt aabb-rot 1))))
                          (list (round (elt res 0)) (round (elt res 1))
                                (funcall door-shift-fun (elt d 2))
                                (elt d 3))))
                    doors)))))

(defmethod extend-x (object)
  (with-accessors ((w w) (pixel-scale pixel-scale)) object
    (incf w pixel-scale)))

(defmethod extend-y (object)
  (with-accessors ((h h) (pixel-scale pixel-scale)) object
    (incf h pixel-scale)))

(defmethod max-color ((object lab-room))
  (let ((max-color '(0 0 0)))
    (labels ((df (obj)
               (when (and
                      (> (first (color obj)) (first max-color))
                      (> (second (color obj)) (second max-color))
                      (> (third (color obj)) (third max-color)))
                 (setf max-color (color obj)))))

      (df object)
      (dfs object #'df)

      max-color)))

(defmethod occupied-rate ((object lab-room))
  (with-accessors ((matrix shared-matrix)) object
   (let ((occupied (loop
                      for i across (matrix:data matrix)
                      when (not (null i))
                      count i))
         (total (* (map-w object) (map-h object))))
     (/ occupied total))))

(defmacro with-restore-map ((map) &body body)
  `(unwind-protect
        (progn ,@body)
     (clear-mat ,map)
     (room->mat ,map)))

(defmethod dump ((object lab-room) (file string) &key (draw-door-to-nowhere t)
                 (draw-seed nil) (draw-id nil))
  (with-open-file (stream file :direction :output :if-exists :supersede
                          :if-does-not-exist :create)
    (dump object stream :draw-door-to-nowhere draw-door-to-nowhere
          :draw-seed draw-seed :draw-id draw-id)))

(defmethod dump ((object lab-room) (stream stream) &key (draw-door-to-nowhere t)
                 (draw-seed nil) (draw-id nil))
  (clear-mat object)
  (room->mat object :draw-door t :draw-door-to-nowhere draw-door-to-nowhere
             :draw-border t :draw-seed draw-seed :draw-id draw-id)
  (matrix:loop-matrix ((shared-matrix object) x y)
    (when (null (matrix:matrix-elt (shared-matrix object) y x))
      (setf (matrix:matrix-elt (shared-matrix object) y x) +black+)))
  (with-restore-map (object)
    (format stream (pixmap:matrix->ppm* (shared-matrix object)))))

(defmethod debug-dump ((object lab-room) file &key (draw-door-to-nowhere nil)
                 (draw-seed t) (draw-id t))
  (with-open-file (stream file :direction :output :if-exists :supersede
                          :if-does-not-exist :create)
    (clear-mat object)
    (debug-room->mat object :draw-door t :draw-door-to-nowhere draw-door-to-nowhere
               :draw-border t :draw-seed draw-seed :draw-id draw-id)
    (matrix:loop-matrix ((shared-matrix object) x y)
      (when (null (matrix:matrix-elt (shared-matrix object) y x))
        (setf (matrix:matrix-elt (shared-matrix object) y x) +black+)))
    (with-restore-map (object)
      (format stream (pixmap:matrix->ppm* (shared-matrix object)
                                          (first (max-color (get-root object))))))))

(defmethod dump-dot ((object lab-room) file)
  (with-open-file (stream file :direction :output :if-exists :supersede
                          :if-does-not-exist :create)
    (clear-mat object)
    (s-dot:render-s-dot file "ps" (room->dot object))))

(defun generate (size &key (scale-fact 1)
                        (func-sigma-w   #'(lambda (x a) (declare (ignore a)) (+ 10 x)))
                        (func-sigma-h   #'(lambda (x a) (declare (ignore a)) (+ 10 x)))
                        (func-door      #'(lambda (x a) (declare (ignore a)) (if (< x 1) 3 4)))
                        (func-win       #'(lambda (x a) (declare (ignore a)) (if (< x 1) 3 4)))
                        (func-furniture #'(lambda (x a) (declare (ignore a)) (if (< x 1) 3 4)))
                        (root (make-instance 'lab-room
                                             :shared-matrix (matrix:gen-matrix-frame size size))))
  (let* ((*queue* nil)
         (*generation-count* 0)
         (*ids* 0)
         (*bfs-visited* '()))
    (and func-sigma-h   (update-params (get-root root) :sigmah func-sigma-h nil))
    (and func-sigma-w   (update-params (get-root root) :sigmaw func-sigma-w nil))
    (push root *queue*)
    (add-doors root)
    (room->mat root)
    (do ()
        ((not (and
               (< *generation-count* +max-generation+)
               (<= (occupied-rate (get-root root))
                   (get-param-gen (get-root root) max-occupied-rate)))))
      (let ((room (grow-step)))
        (and func-sigma-h   (update-params (get-root root) :sigmah func-sigma-h room))
        (and func-sigma-w   (update-params (get-root root) :sigmaw func-sigma-w room))
        (and func-door      (update-params (get-root root) :max-door-num func-door room))
        (and func-win       (update-params (get-root root) :max-windows-num func-win room))
        (and func-furniture (update-params (get-root root)
                                           :max-furnitures-num-fn
                                           func-furniture room))
        (when room
          (grow-step-windows room))))
    (clean-unused-doors root :purge-blind-doors nil)
    (clean-border-doors root)
    (clean-superimposed-windows-doors root)
    (when (> scale-fact 0)
       (scale root scale-fact))
    (clear-mat root)
    (grow-furnitures root)
    (clean-furniture-near-door root)
    (clear-mat root)
    root))

(defun gen-simple-room (size &key (furniture-fn nil))

  (let ((res (make-instance 'lab-room
                            :x     0
                            :y     0
                            :w     (1- size)
                            :h     (1- size)
                            :doors (list (list (lcg-next-in-range 1 (1- size))
                                               0
                                               :n
                                               :outside)))))
    (and furniture-fn
         (update-params (get-root res) :max-furnitures-num-fn furniture-fn res))
    (setf (shared-matrix res) (matrix:gen-matrix-frame size size))
    (clean-and-redraw-mat res)
    (grow-furnitures res)
    (clean-and-redraw-mat res)
    res))
