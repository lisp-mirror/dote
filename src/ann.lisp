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

(in-package :ann)

(defun random-0to1 ()
  (num-utils:lcg-next01))

(defun random-1to1 ()
  (1- (* (num-utils:lcg-next01) 2.0)))

(defun random-simmetric (delta)
  (- delta (* (num-utils:lcg-next01) (* 2.0 delta))))

(defun sigmoid (x &optional (p 1))
  (let ((p-actual (if (stringp p) (parse-number p) p)))
    (/ 1 (+ 1 (exp (- (/ x p-actual)))))))

(defun string->activation-function (str)
  (ecase (alexandria:make-keyword (format nil "~@:(~a~)" str))
    (:sigmoid
     #'sigmoid)))

(defun sse (measure expected)
  (reduce #'+ (mapcar #'(lambda (m e) (expt (- m e) 2)) measure expected) 
	  :initial-value 0))

(defun add-jitter (list noise)
  (mapcar #'(lambda (v) (+ v (random-simmetric noise))) list))

(alexandria:define-constant +ann-el+ "ann" :test #'string=)

(alexandria:define-constant +layer-el+ "layer" :test #'string=)

(alexandria:define-constant +layers-el+ "layers" :test #'string=)

(alexandria:define-constant +neuron-el+ "neuron" :test #'string=)

(alexandria:define-constant +neurons-el+ "neurons" :test #'string=)

(alexandria:define-constant +weight-el+ "weight" :test #'string=)

(alexandria:define-constant +momentum-att+ "activation-function-param" 
  :test #'string=)

(alexandria:define-constant +activation-fun-att+ "activation-function" :test #'string=)

(alexandria:define-constant +activation-function-param-att+ "activation-function-param" 
  :test #'string=)

(defparameter *jitter-noise* 1d-1)

(defclass neuron ()
  ((weights
    :initform '()
    :initarg :weights
    :accessor weights
    :type list)
   (prev-dweights
    :initform '()
    :initarg :prev-dweights
    :accessor prev-dweights
    :type list)
   (activation-threshold
    :initform (random-1to1)
    :initarg :activation-threshold
    :accessor activation-threshold
    :type real)
   (prev-dactivation-threshold
    :initform 0
    :initarg :prev-dact
    :accessor prev-dact
    :type real)
   (momentum
    :initform 0.9
    :initarg :momentum
    :accessor momentum
    :type real)
   (activation-function
    :initform nil
    :initarg :activation-function
    :accessor activation-function
    :type function)))

(defun make-neuron-weights (neuron weights-list)
  (when weights-list
    (mapcar #'(lambda (w) (push w (weights neuron))) 
	    (subseq weights-list 0 (1- (length weights-list))))
    (setf (weights neuron) (reverse (weights neuron)))
    (setf (activation-threshold neuron) (car (last weights-list))))
  (setf (prev-dweights neuron) 
	(map-into (make-list (length (weights neuron))) #'(lambda () 0))))

(defmethod initialize-instance :after ((object neuron) &key (weights-list nil) &allow-other-keys)
  (make-neuron-weights object weights-list))

(defmethod print-object ((object neuron) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "weigths: ~{~a ~} activation: ~a activation function: ~a momentum ~a" 
	    (weights object) (activation-threshold object)
	    (activation-function object)
	    (momentum object))))

(defmethod to-sexp ((object neuron))
  (list (alexandria:make-keyword +weight-el+) (concatenate 'list  
							   (weights object) 
							   (list 
							    (activation-threshold object)))))

(defmethod from-sexp ((object neuron) sexp)
  (make-neuron-weights object (getf sexp (alexandria:make-keyword +weight-el+)))
  object)

(defgeneric update-weights-neuron (object learning-rate errors outputs-next-layer &key gradient-only))

(defgeneric increase-all-weights (object weigths &key add-momentum))

(defgeneric get-gradient-neuron (object learning-rate errors outputs-next-layer &key add-momentum))

(defgeneric fire (object input))

(defmethod clone ((object neuron))
  (make-instance 'neuron 
		 :weights (copy-list (weights object))
		 :prev-dweights (copy-list (prev-dweights object))
		 :activation-threshold (activation-threshold object)
		 :activation-function (activation-function object)
		 :prev-dact            (prev-dact object)
		 :momentum (momentum object)))

(defmethod get-gradient-neuron ((object neuron) learning-rate error output-next-layers 
				&key (add-momentum t))
  (with-accessors ((pdwght prev-dweights) (pdt prev-dact)
		   (mom momentum)) object
    (let* ((dweights (mapcar #'(lambda (out) (* learning-rate out error))
			     output-next-layers))
	   (dthreshold (* -1 learning-rate error))
	   (gradient-weights (if add-momentum
			     (mapcar #'(lambda (dw pdw) (+ dw (* mom pdw))) 
				     dweights pdwght)
			     dweights))
	   (gradient-activation (if add-momentum
				    (+ dthreshold (* mom pdt))
				    dthreshold)))
      (values gradient-weights gradient-activation dweights dthreshold))))

;output for each neuron (vector) error is a number
(defmethod update-weights-neuron ((object neuron) learning-rate error output-next-layers 
				  &key (gradient-only nil))
  (with-accessors ((pdwght prev-dweights) (pdt prev-dact)
		   (mom momentum)) object
    (multiple-value-bind (gradient-weights gradient-activation dweights dthreshold)
	(get-gradient-neuron object learning-rate error output-next-layers :add-momentum t)
      (if gradient-only
	  (values gradient-weights gradient-activation)
	  (progn
	    (setf (weights object)
		  (mapcar #'(lambda (w grad) (+ w grad))
			  (weights object) gradient-weights))
	    (setf (activation-threshold object) 
	     	  (+ (activation-threshold object) gradient-activation))
	     (setf pdwght dweights)
	     (setf pdt dthreshold))))))

(defmethod increase-all-weights ((object neuron) weights &key (add-momentum t))
  (with-accessors ((ann-wght weights) (act-thrs activation-threshold)
		   (pdwght prev-dweights) (pdt prev-dact)
		   (mom momentum)) object
    (let* ((gradient-weights (first weights))
	   (gradient-activation (second weights))
	   (dweights (if add-momentum
			 (mapcar #'(lambda (dw pdw) (+ dw (* mom pdw))) 
				 gradient-weights pdwght)
			 gradient-weights))
	   (dactivation (if add-momentum
			   (+ gradient-activation (* mom pdt))
			   gradient-activation)))
      (if (= (length dweights) (length ann-wght))
	  (progn
	    (setf pdwght gradient-weights)
	    (setf pdt gradient-activation)
	    (setf ann-wght (mapcar #'(lambda (w dw) (+ w dw)) ann-wght dweights))
	    (setf act-thrs (+ act-thrs dactivation)))
	  (error 'different-length-error :seq1 dweights :seq2 ann-wght)))))

(defmethod fire ((object neuron) (input list))
  (with-accessors ((weights weights) (threshold activation-threshold)
		   (act-fun activation-function)) object
    (if (= (length weights) (length input))
	(funcall act-fun
		 (reduce #'+ (mapcar #'(lambda (w i) (* w i)) weights input) 
			 :initial-value (- threshold)))
	(error 'different-length-error :seq1 weights :seq2 input))))

(defmethod serialize ((object neuron))
  (let ((weights (mapcar #'(lambda (w) (xmls:make-xmlrep +weight-el+ :children (list w)))
		 (weights object)))
	(act (list 
	      (xmls:make-xmlrep +weight-el+ :children (list (activation-threshold object)))))
	(root (xmls:make-xmlrep +neuron-el+)))
    (mapc #'(lambda (w) (xmls:xmlrep-add-child! root w)) (append weights act))
    root))

(defclass ann-layer ()
  ((neurons
    :initarg :neurons
    :accessor neurons
    :initform '()
    :type list)))

(defmethod clone ((object ann-layer))
  (make-instance 'ann-layer :neurons (mapcar #'clone (neurons object))))

(defmethod initialize-instance :after ((object ann-layer) &key (weights-lists '()) 
				       (activation-function #'sigmoid) &allow-other-keys)
  (mapcar
   #'(lambda (weight-list)
       (push 
	(make-instance 'neuron 
		       :weights-list weight-list 
		       :activation-function activation-function)
	(neurons object)))
   weights-lists)
  (setf (neurons object) (neurons object)))

(defmethod print-object ((object ann-layer) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "layer: ~{~a~%~}" (neurons object))))

(defmethod fire ((object ann-layer) (input list))
  (mapcar #'(lambda (n) (fire n input)) (neurons object)))

(defmethod serialize ((object ann-layer))
  (let ((s-neurons (mapcar #'(lambda (n) (serialize n)) (neurons object)))
	(root (xmls:make-xmlrep +layer-el+)))
    (mapc #'(lambda (w) (xmls:xmlrep-add-child! root w)) s-neurons)
    root))

(defmethod to-sexp ((object ann-layer))
  (list (alexandria:make-keyword +layer-el+)
	(mapcar #'(lambda (n) (to-sexp n)) (neurons object))))

(defmethod from-sexp ((object ann-layer) sexp)
  (let ((neuron-sexp (getf sexp (alexandria:make-keyword +layer-el+))))
    (setf (neurons object)
	  (mapcar #'(lambda (n) 
		      (let ((neuron (make-instance 'neuron)))
			(from-sexp neuron n)
			neuron))
		  neuron-sexp)))
  object)
    
(defclass ann ()
  ((layers
    :initform '()
    :initarg :layers
    :accessor layers
    :type list)
   (outputs
    :initform '()
    :initarg :outputs
    :accessor outputs)
   (input
    :initform '()
    :initarg :input
    :accessor input)
   (total-sse
    :initform 0
    :initarg :total-sse
    :accessor total-sse)
   (activation-function-name
    :initform ""
    :initarg :activation-function-name
    :accessor activation-function-name
    :type string)
   (activation-function-param-name
    :initform ""
    :initarg :param-name
    :accessor param-name
    :type string)))

(defmethod to-sexp ((object ann))
  (list (alexandria:make-keyword +activation-fun-att+)
	(activation-function-name object)
	(alexandria:make-keyword +activation-function-param-att+)
	(param-name object)
	(alexandria:make-keyword +layers-el+)
	(mapcar #'to-sexp (layers object))))

(defmethod from-sexp ((object ann) sexp)
  (setf (activation-function-name object) 
	(getf sexp (alexandria:make-keyword +activation-fun-att+))
	(param-name object)
	(getf sexp (alexandria:make-keyword +activation-function-param-att+))
	(layers object)
	(mapcar #'(lambda (l) 
		    (let ((layer (make-instance 'ann-layer))) 
		      (from-sexp layer l)))
		(getf sexp (alexandria:make-keyword +layers-el+))))
  (mapc #'(lambda (layer)
	    (mapc #'(lambda (neuron) 
		      (setf (activation-function neuron)
			    (lambda (i) 
			      (apply
			       (string->activation-function 
			     	(activation-function-name object))
			       (list i (param-name object))))))
		  (neurons layer)))
	(layers object))
  object)

(defmethod clone ((object ann))
  (make-instance 'ann 
		 :layers (mapcar #'clone (layers object))
		 :outputs (copy-tree (outputs object))
		 :input (copy-list (input object))
		 :total-sse (total-sse object)))

(defmethod print-object ((object ann) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "ann: ~{~a~%~}" (layers object))))

(defgeneric feed (object input &optional layers snapshot))

(defgeneric update-weights (object layer-pos learning-rate errors &key gradient-only))

(defgeneric get-gradients (object layer-pos learning-rate errors &key add-momentum))

(defgeneric error-output (object expected))

(defgeneric calculate-error-hidden (object layer-pos expected input))

(defgeneric backpropagation (object learning-rate input expected))

(defgeneric train (object input-set expected-set &key min-sse learning-rate stream
			  flush-output verbose-output))

(defgeneric save-ann (object path))

(defmethod increase-all-weights ((object ann) weights &key (add-momentum t))
  (let ((nw-dweights  (reduce #'(lambda (old new)
				  (mapcar #'(lambda (lay-n1 lay-n2)
					      (mapcar #'(lambda (n1 n2)
							  (list
							   (mapcar #'(lambda (w1 w2)
								       (+ w1 w2))
								   (first n1) (first n2))
							   (+ (second n1) (second n2))))
						      
						      lay-n1 lay-n2))
					  old new))
			      weights)))
    (do* ((weights-layer nw-dweights (rest weights-layer))
	  (ctl 0 (1+ ctl)))
	 ((not weights-layer))
      (mapcar #'(lambda (n wn) (increase-all-weights n wn :add-momentum add-momentum))
	      (neurons (nth ctl (layers object))) (first weights-layer)))))

;; error are for each neuron
(defmethod update-weights ((object ann) layer-pos learning-rate errors
			   &key (gradient-only nil))
  (let ((layer (nth layer-pos (layers object))))
    (mapcar #'(lambda (n err) (progn ;(format t "  layer~%")
				     (update-weights-neuron n learning-rate err
							    (if (> layer-pos 0)
								(nth (1- layer-pos) (outputs object))
								(input object))
							    :gradient-only gradient-only)))
	    (neurons layer)
	    errors)))

(defmethod get-gradients ((object ann) layer-pos learning-rate errors &key (add-momentum t))
  (let ((layer (nth layer-pos (layers object))))
    (mapcar #'(lambda (n err) (multiple-value-list
			       (get-gradient-neuron n learning-rate err
						    (if (> layer-pos 0)
							(nth (1- layer-pos) (outputs object))
							(input object))
						    :add-momentum add-momentum)))
	    (neurons layer)
	    errors)))

;; E output = | (t_k - o_k) * o_k(1 - o_k) ...| 
(defmethod error-output ((object ann) expected) ; list of errors for each neuron
  (let ((output-ann (car (last (outputs object)))))
    (mapcar #'(lambda (out exp) (* (- exp out) out (- 1 out)))
	    output-ann expected)))

;; E hidden = | o_j * (1 - o_j) * sum E_k * w_jk ...| 
(defmethod calculate-error-hidden ((object ann) layer-pos expected input)
  (let* ((errors (if (= layer-pos 0) ; last layer
		     (error-output object expected)
		     (calculate-error-hidden object (1+ layer-pos) expected input)))
	 (weights-prev (mapcar #'weights (neurons (nth (1+ layer-pos) (layers object)))))
	 (output       (nth layer-pos (outputs object)))
	 (errors-hidden-sum (mapcar #'(lambda (weights)
					(reduce #'+ (mapcar #'(lambda (err w) (* err w)) 
							    errors weights) 
						:initial-value 0))
				    weights-prev))
	 (errors-hidden (mapcar #'(lambda (out errsum)
				    (* out (- 1 out) errsum))
				output errors-hidden-sum)))
    errors-hidden))

(defmethod backpropagation ((object ann) learning-rate input expected)
  (setf (input object) (add-jitter input *jitter-noise*))
  (feed object (input object))
  (setf (total-sse object) (+ (total-sse object)
			      (sse (car (last (outputs object))) expected)))
  (let ((err-out (error-output object expected)))
    (update-weights object (1- (length (layers object))) learning-rate err-out)
    (loop for lp from (- (length (layers object)) 2) downto 0 by 1 do
	 (let* ((err-hidden (calculate-error-hidden object lp expected input)))
	   (update-weights object lp learning-rate err-hidden)))))

(defmethod feed ((object ann) (input list) 
		 &optional (layers (layers object)) (snapshot '()))
		 
  (if (not (null (rest layers)))
      (feed object (fire (first layers) input) (rest layers) (append
							      snapshot
							      (list (fire (first layers) input))))
      (setf (outputs object) (append snapshot (list (fire (first layers) input))))))

(defmethod train ((object ann) input-set expected-set
		  &key (min-sse .0001) (learning-rate 1e-3) (stream *standard-output*)
		  (flush-output nil) (verbose-output t) (max-iter-number 200000))
  (setf (total-sse object) (1+ min-sse))
  (do ((ct-iter 0 (1+ ct-iter)))
      ((or
	(< (total-sse object) min-sse)
	(>= ct-iter max-iter-number)))
    (if stream
	(let ((output (if verbose-output (outputs object) nil)))
	  (format stream "~a~@[ out ~a~]~%" (total-sse object) output)
	  (when flush-output
	    (finish-output stream))))
    (setf (total-sse object) 0)
    (mapcar #'(lambda (input expected)
		(backpropagation object learning-rate input expected))
	    input-set expected-set)))

(defmethod serialize ((object ann))
  (let* ((s-layers (mapcar #'(lambda (l) (serialize l)) (layers object)))
	 (attribs (list (list +activation-fun-att+ (activation-function-name object))
			(list +activation-function-param-att+ (param-name object))))
	 (root (xmls:make-xmlrep +ann-el+ :attribs attribs)))
    (mapc #'(lambda (w) (xmls:xmlrep-add-child! root w)) s-layers)
    root))

(defmethod save-ann ((object ann) path)
  (with-open-file (stream path :direction :output :if-exists :supersede)
    (xmls:write-xml (serialize object) stream :indent t)))

(defun xmls->ann (xmllist)
  (macrolet ((with-list-tag ((tag children) &body body)
	       `(mapcar #'(lambda (n) 
			    (progn ,@body))
			(remove-if-not 
			 #'(lambda (child)
			     (if (xmls:xmlrep-tagmatch ,tag child)
				 t
				 nil))
			 ,children)))
	     (get-other-values (name pos children)
	       `(with-tagmatch (,name (nth ,pos ,children))
		  (first (xmlrep-children (nth ,pos ,children))))))
    (labels ((p-layer (node)
	       (let* ((layer-tag  (with-tagmatch (+layer-el+ node)))
		      (children   (xmls:xmlrep-children node))
		      (neurons    (with-list-tag (+neuron-el+ children) (identity n)))
		      (weights    (mapcar #'(lambda (nr) (p-neuron nr)) neurons)))
		 (declare (ignore layer-tag))
		 weights))
	     (p-neuron (node)
	       (let* ((neuron-tag  (with-tagmatch (+neuron-el+ node)))
		      (children   (xmls:xmlrep-children node))
		      (weigths     (with-list-tag (+weight-el+ children)
				     (parse-number (first (xmls:xmlrep-children n))))))
		 (declare (ignore neuron-tag))
		 (copy-list weigths))))
      (with-tagmatch (+ann-el+ xmllist)
	(let* ((act-funname-raw (with-attribute (+activation-fun-att+ xmllist)))
	       (act-funname (string->activation-function act-funname-raw))

	       (act-fun-param (with-attribute (+activation-function-param-att+ xmllist)))
	       (weights	 (mapcar #'(lambda (n) (p-layer n))
				 (xmls:xmlrep-children xmllist)))
	       (act-fun (function (lambda (i) (apply act-funname (list i act-fun-param))))))
	  (make-instance 'ann 
			 :activation-function-name act-funname-raw
			 :param-name act-fun-param
			 :layers (mapcar #'(lambda (w) (make-instance 'ann-layer
								      :activation-function act-fun
								      :weights-lists (reverse w)))
					 weights)))))))
(defun load-ann (path)
  (with-open-file (stream path :direction :input :if-does-not-exist :error)
    (xmls->ann (xmls:parse stream))))

(defun quick-ann-skeleton (path input-num hidden-num output-num act-name act-par)
  (labels ((random-weights-list (lght)
	     (map-into (make-list (1+ lght)) #'(lambda () (random-1to1)))))
    (let ((ann (make-instance 'ann :param-name act-par :activation-function-name act-name))
	  (output-layer (make-instance 'ann-layer
				       :neurons (loop for i from 0 below output-num collect
						     (make-instance 'neuron 
								    :weights-list (random-weights-list hidden-num)))))
	(hidden-layer (make-instance 'ann-layer
				     :neurons (loop for i from 0 below hidden-num collect
							 (make-instance 'neuron
									:weights-list (random-weights-list input-num)))))) 
      (setf (layers ann) (list hidden-layer output-layer))
      (save-ann ann path))))

(defun gen-blank-hopfield-ann (pattern-length)
  (let ((graph (make-instance 'graph:matrix-graph :size pattern-length)))
    (setf (graph:matrix graph)
	  (matrix:map-matrix (graph:matrix graph)
			     #'(lambda (el) (declare (ignore el)) 0)))
    graph))

(defgeneric memorize (object pattern))

(defgeneric recall (object pattern &key old-pattern lock-pattern))

(defmethod memorize ((object graph:matrix-graph) (pattern matrix))
  (memorize object (data pattern)))

(defmethod memorize ((object graph:matrix-graph) (pattern sequence))
  (loop for i from 0 below (length pattern) do
       (loop for j from i below (matrix:width (graph:matrix object)) do
	      (when (/= i j)
		(setf (graph:traverse-cost object i j)
		      (+ (graph:traverse-cost object i j)
			 ;;(2Vi - 1)(2Vj - 1)
			 (* (- (* 2 (elt pattern i)) 1)
			    (- (* 2 (elt pattern j)) 1))))))))

(defun cross-product (a b)
  (reduce #'+ (mapcar #'* a b)
	  :initial-value 0))

(defmethod recall ((object graph:matrix-graph) (pattern matrix)
		   &key (lock-pattern nil) (old-pattern nil))
  (recall object (coerce (data pattern) 'list) 
	  :old-pattern old-pattern
	  :lock-pattern lock-pattern))

(defmethod recall ((object graph:matrix-graph) (pattern list) 
		   &key (lock-pattern nil) (old-pattern nil))
  (let ((random-visit  (misc:shuffle 
			(loop for i from 0 below (length pattern) collect i))))
    (labels ((update-neuron (matrix pattern row)
	       (let* ((list-weigths (matrix:row->sequence matrix row 'list))
		      (cross-product (cross-product list-weigths pattern)))
		      (if (< cross-product 0)
			  0
			  1))))
      (loop for i from 0 below (length random-visit) do
	   (when (or (null lock-pattern) (elt lock-pattern (elt random-visit i)))
	     (setf (elt pattern (elt random-visit i))
		   (update-neuron (graph:matrix object) pattern 
				  (elt random-visit i)))))
      (if (or 
       	     (null old-pattern)
       	     (not (subsetp old-pattern (intersection old-pattern pattern))))
       	  (recall object pattern :old-pattern pattern :lock-pattern lock-pattern)
	  pattern))))

(defun test ()
  (let ((mat (gen-blank-hopfield-ann 5))
	(mat2 (gen-blank-hopfield-ann 0))
	(pattern '(1 0 0 1 1))
	(pattern2 '(1 0 1 0 1)))
    (format t "pattern ~%~a~%" pattern)
    (format t "~a~%" mat)
    (memorize mat pattern)
    (memorize mat pattern2)
    (format t "~a~%" mat)
    (recall mat '(1 0 1 1 0) :lock-pattern '(t t t nil nil))
    (from-sexp (graph:matrix mat2) (to-sexp (graph:matrix mat)))
    mat2))
