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

(in-package :id3)

(define-constant +min-cardinality-partition+ 2     :test #'=)

(define-constant +epsilon+                   1.e-3 :test #'=)

(defun shuffle-table (table)
  (misc:shuffle table))

(defun all-values-set (table attribute-position)
  (let ((set nil))
    (loop for row in table do
	 (setf set (adjoin (nth attribute-position row) set)))
    set))

(defun table-height (table)
  (length table))

(defun table-width (table)
  (length (first table)))

(defun sort-table-by-attribute-position (table position)
  (sort table #'< :key #'(lambda (a) (nth position a))))

(defun sort-table-by-attribute (table attributes sort-column)
  (let ((position (position sort-column attributes)))
    (if position
	(sort-table-by-attribute-position table position)
	nil)))

(defun split-table-by-row-position (table row-position)
  (if (and (< row-position (table-height table))
	   (> row-position 0))
      (let ((t1 nil)
	    (t2 nil))
	(loop for i from 0 below (table-height table) do
	     (if (< i row-position)
		 (setf t1 (concatenate 'list t1 (list (nth i table))))
		 (setf t2 (concatenate 'list t2 (list (nth i table))))))
	(values t1 t2))
      nil))

(defun split-by-continuous-value (table attributes attribute
				     &key (decision (alexandria:lastcar attributes)))
  (let ((sorted-table (sort-table-by-attribute (copy-tree table) attributes attribute))
	(splitter-row 0)
	(max-gain -1))
    (loop for i from 1 below (table-height sorted-table) do
	 (multiple-value-bind (t1 t2)
	     (split-table-by-row-position sorted-table i)
	   (let ((gain (information-gain-splitted sorted-table
						  attributes attribute decision
						  (list t1 t2))))
	     (when (> gain max-gain)
	       (setf max-gain gain
		     splitter-row i)))))
    (multiple-value-bind (t1 t2)
	(split-table-by-row-position sorted-table splitter-row)
      (let* ((position (position attribute attributes))
	     (max-value (nth position (alexandria:lastcar t1)))
	     (min-value (nth position (first t2))))
	(values (list t1 t2) max-gain (alexandria:mean (list min-value max-value)))))))

(defun split-by-attribute-position (table position)
  (let ((all-values (all-values-set table position))
	(tables      nil))
    (loop for i in all-values do
	 (push (remove-if #'null
			  (mapcar #'(lambda (row)
				      (if (eql i (nth position row))
					  row
					  nil))
				  table))
	       tables))
    tables))

(defun example-get-by-attribute-position (example position)
  (if (and (>= position 0)
	   (< position (length example)))
      (nth position example)
      nil))

(defun example-get-by-attribute-value (example attributes attribute)
  (let ((position (position attribute attributes)))
    (if position
	(example-get-by-attribute-position example position)
	nil)))

(defmacro ecase-attribute ((table attributes attribute) &body body)
  `(etypecase (get-test-results ,table ,attributes ,attribute)
     ,@body))

(defun split-by-attribute-value (table attributes split)
  (let ((position (position split attributes)))
    (if position
	(ecase-attribute (table attributes split)
	  (symbol
	   (split-by-attribute-position table position))
	  (number
	   (split-by-continuous-value table attributes split)))
	'nil)))

(defun count-attributes (table attributes count-attribute)
  (let ((position (position count-attribute attributes)))
    (if position
	(let ((splitted (split-by-attribute-position table position))
	      (results nil))
	  (loop for table in splitted do
	       (setf results
		     (acons (nth position (first table)) (table-height table) results)))
	  results)
	nil)))

(defun entropy (table attributes decisions)
  (let* ((count-attributes (count-attributes table attributes decisions))
	 (count-set (table-height table))
	 (frequencies (loop for i in count-attributes collect
			   (/ (cdr i) count-set))))
    (loop for i in frequencies sum
	 (- (* i (log i 2))))))

(defun split-info (original-table tables)
  (let* ((card-table      (table-height original-table))
	 (card-partitions (loop for i in tables collect (table-height i)))
	 (ratios          (loop for card-partition in card-partitions collect
			       (/ card-partition card-table)))
	 (res            (loop for i in ratios sum
			      (- (* i (log i 2))))))
    res))

(defun gain-ratio (table attributes attribute
 			 &key (decision (alexandria:lastcar attributes)))
  (gain-ratio-splitted table attributes attribute decision
			     (ecase-attribute (table attributes attribute)
			       (symbol
				(split-by-attribute-value table attributes attribute))
			       (number
				(split-by-continuous-value table attributes attribute)))))

(defun information-gain (table attributes attribute
			  &key (decision (alexandria:lastcar attributes)))
  (information-gain-splitted table attributes attribute decision
			     (ecase-attribute (table attributes attribute)
			       (symbol
				(split-by-attribute-value table attributes attribute))
			       (number
				(split-by-continuous-value table attributes attribute)))))

(defun information-gain-splitted (original-table attributes attribute decision tables)
  (let* ((whole-entropy    (entropy original-table attributes decision))
	 (position         (position attribute attributes))
	 (count-attributes (loop for i in tables collect (table-height i)))
	 (count-set        (table-height original-table))
	 (count-card-ok    (count-if #'(lambda (a) (>= a +min-cardinality-partition+))
				     count-attributes)) ;; we could skip the rest if this is < 2
	 (frequencies      (loop for i in count-attributes collect
				(cons i (/ i count-set))))
	 (entropies        (loop for i in tables collect
				(cons (nth position (first i))
				      (entropy i attributes decision))))
	 (gain             (- whole-entropy (loop for i from 0 below (length entropies) sum
						 (* (cdr (nth i entropies))
						    (cdr (nth i frequencies)))))))
    (if (< count-card-ok 2)
	+epsilon+
	gain)))

(defun gain-ratio-splitted (original-table attributes attribute decision tables)
  (let* ((split-info (split-info original-table tables))
	 (gain       (information-gain-splitted original-table
						attributes
						attribute
						decision
						tables)))
    (if (not (num:epsilon= split-info 0.0))
	(/ gain split-info)
	(- +epsilon+))))

(defun classes-list (table &optional (classes-pos (1- (table-width table))))
  (let ((res '()))
    (loop for i in table do
	 (pushnew (nth classes-pos i) res :test #'equal))
    res))

(defclass decision-tree (m-tree)
  ((path
    :initform '()
    :initarg :path
    :accessor path)
   (decisions-count
    :initform '()
    :initarg :decisions-count
    :accessor decisions-count)))

(defmethod marshal:class-persistant-slots ((object decision-tree))
  (append '(path
	    decisions-count)
	  (call-next-method)))

(defmethod serialize ((object decision-tree))
  (format nil "~s" (marshal:marshal object)))

(defmethod deserialize ((object decision-tree) file)
  (declare (ignore object))
  (marshal:unmarshal (read-from-string (filesystem-utils:slurp-file file))))

(defgeneric test-example (object attributes example))

(defgeneric pachinko (object examples attributes))

(defgeneric statistical-error (object))

(defgeneric count-misclassified (object))

(defgeneric count-classified (object))

(defgeneric prune-node (object))

(defgeneric update-decisions-count (object decision))

(defgeneric reset-all-decisions-count (object))

(defgeneric get-children-decisions-count (object))

(defmethod print-object ((object decision-tree) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~%")
    (pprint-tree object stream)))

(defmethod pprint-tree ((object decision-tree) stream &optional (level 0) (parent-length 0)
			(other-data nil))
  (declare (ignore parent-length other-data))
  (with-accessors ((data data) (children children) (path path) (parent parent)
		   (decisions-count decisions-count)) object
    (labels ((indent (level &optional (char " ")) (make-list level :initial-element char))
	     (get-length (&rest data) (length (format nil "~{~a~}" data)))
	     (branch-format () "~@[ if ~a then~]~%")
	     (branch-format-less () "~@[ if <= ~,2,f then~]~%")
	     (branch-format-more () "~@[ if >  ~,2,f then~]~%")
	     (node-string (data path &key (type '=))
	       (ecase type
		 (=
		  (format nil (concatenate 'string "~{~a~}~a"
					   (branch-format))
			  (indent level) data path))
		 (<=
		  (format nil (concatenate 'string "~{~a~}~a"
					   (branch-format-less))
			  (indent level) data path))
		 (>
		  (format nil (concatenate 'string "~{~a~}~a"
					   (branch-format-more))
			  (indent level) data path))))
	     (length-node-string (data path &key (type '=))
	       (length (node-string data path :type type))))
      (if (leafp object)
	  (format stream (node-string data nil))
	  (let ((firstp t))
	    (map nil #'(lambda (c p)
		      (let ((type (cond
				    ((and firstp (numberp p))
				     '<=)
				    ((numberp p)
				     '>)
				    (t '=))))
			(when firstp (setf firstp nil))
			(format stream (node-string data p :type type))
			(pprint-tree c stream (length-node-string data p :type type))))
		  children path))))))

(alexandria:define-constant +data-el+ "data"                    :test #'string=)

(alexandria:define-constant +path-el+ "path"                    :test #'string=)

(alexandria:define-constant +decisions-count+ "decisions-count" :test #'string=)

(defmethod to-sexp ((object decision-tree))
  (let ((serialized (list nil)))
    (labels ((%serialize (decisions &optional (tree nil))
	       (nappend-child tree (list
				    (alexandria:make-keyword +data-el+) (data decisions)
				    (alexandria:make-keyword +path-el+) (path decisions)
				    (alexandria:make-keyword +decisions-count+)
				    (decisions-count decisions)))
	       (map nil #'(lambda (c) (%serialize c (alexandria:lastcar tree)))
		     (children decisions))))
      (%serialize object serialized)
      (second serialized))))

(defmethod from-sexp ((object decision-tree) sexp)
  (labels ((dfs (node decision-node)
	     (format t "node ~a~%" (car node))
	     (multiple-value-bind (tree new-child)
		 (add-child decision-node
			    (make-instance 'decision-tree
					   :data (getf (car node)
						       (alexandria:make-keyword +data-el+))
					   :path (getf (car node)
						       (alexandria:make-keyword +path-el+))
					   :decisions-count
					   (getf (car node)
						 (alexandria:make-keyword +decisions-count+))))
	       (declare (ignore tree))
	       (loop for i in (cdr node) do
		    (dfs i new-child)))))
    (dfs sexp object)
    (setf (parent (elt (children object) 0)) nil)
    (elt (children object) 0)))

(defmethod leafp ((object decision-tree))
  (null (path object)))

(defmethod update-decisions-count ((object decision-tree) decision)
  (if (assoc decision (decisions-count object))
      (incf (cdr (assoc decision (decisions-count object))))
      (push (cons decision 1) (decisions-count object))))

(defmethod reset-all-decisions-count ((object decision-tree))
  (bottom-up-visit object #'(lambda (n) (setf (decisions-count n)
					      (mapcar #'(lambda (d) (cons (car d) 0))
						      (decisions-count n))))))

(defmethod test-example ((object decision-tree) attributes example)
  (labels((%test-example (node attributes example)
	    (if (leafp node)
		(progn
		  (update-decisions-count node (alexandria:lastcar example)))
		(let* ((res (example-get-by-attribute-value example attributes (data node)))
		       (res-path (if (numberp res)
				     (if (<= res (first (path node)))
					 0
					 1)
				     (position res (path node)))))
		  (%test-example (elt (children node) res-path) attributes example)))))
    (%test-example object attributes example)))

(defmethod pachinko ((object decision-tree) examples attributes)
  (mapcar #'(lambda (example)
	      (test-example object attributes example))
	  examples))

(defmethod statistical-error ((object decision-tree))
  (let* ((leafs-count         (count-leafs          object))
	 (facts-count         (count-classified     object))
	 (misclassified-count (count-misclassified  object))
	 (err                 (+ (/ leafs-count 2) misclassified-count))
	 (stdev-err           (sqrt (/ (* err
					  (- facts-count err))
				       facts-count))))
    (values err stdev-err)))

(defmethod count-misclassified ((object decision-tree))
    (let ((errors 0))
      (top-down-visit object
		      #'(lambda (n)
			  (when (leafp n)
			    (loop for i in (decisions-count n) do
				 (when (and (> (cdr i) 0)
					    (not (equal (car i) (data n))))
				   (incf errors (cdr i)))))))
      errors))

(defmethod count-classified ((object decision-tree))
  (let ((res 0))
    (top-down-visit object
		    #'(lambda (n)
			(incf res
			      (reduce #'(lambda (a b) (+ a (cdr b)))
				      (decisions-count n)
				      :initial-value 0))))
    res))

(defmethod clone ((object decision-tree))
  (let ((root (make-instance 'decision-tree)))
    (labels ((dfs (node-from node-to)
	       (multiple-value-bind (tree new-child)
		   (add-child node-to (make-instance 'decision-tree
						     :data (data node-from)
						     :path (copy-list (path node-from))
						     :decisions-count
						     (copy-tree (decisions-count node-from))))
		 (declare (ignore tree))
		 (loop for i across (children node-from) do
		      (dfs i new-child)))))
      (dfs object root)
      (setf (parent (elt (children root) 0)) nil)
      (elt (children root) 0))))

(defun merge-decisions-counts (alists)
  (let ((results '()))
    (loop for i in alists do
	 (loop for j in i do
	      (if (assoc (car j) results)
		  (setf (cdr (assoc (car j) results))
			(+ (cdr (assoc (car j) results)) (cdr j)))
		  (push j results))))
    results))

(defmethod get-children-decisions-count ((object decision-tree))
  (let ((all-decisions-count '()))
    (top-down-visit object
		    #'(lambda (n) (push (decisions-count n) all-decisions-count)))
    (setf all-decisions-count (merge-decisions-counts all-decisions-count))
    all-decisions-count))

(defmethod prune-node ((object decision-tree))
  (let ((all-decisions-count (sort (get-children-decisions-count object) #'> :key #'cdr)))
    (setf (data object) (car (first all-decisions-count)))
    (setf (children object) #())
    (setf (path object) nil)))

(defun get-test-results (table attributes attribute)
  (let ((position (position attribute attributes)))
    (if position
	(nth position (first table))
	nil)))

(defun row-equal-p (a b)
  "assuming (length a) = (length b) and type of column are congruent (i.e. all the same type)"
  (loop
     for i in (subseq a 0 (1- (length a)))
     for j in (subseq b 0 (1- (length a))) do
       (etypecase i
	 (number
	  (when (not (num:epsilon= i j))
	    (return-from row-equal-p nil)))
	 (symbol
	  (when (not (eq i j))
	    (return-from row-equal-p nil)))))
  t)

(defun find-contradictions (table)
  "return all the facts that are equals, that is same values for each column except for the last
  (the decision). The expression return:
  '((a b c action) ...)
   the action is the one that appear with the maximum frequency."
  (loop for row in table collect
       (let* ((contradictions (remove-if-not #'(lambda (a) (row-equal-p row a)) table))
	      (all-decisions (mapcar #'alexandria:lastcar contradictions))
	      (decisions     '()))
	 (dolist (decision all-decisions)
	   (pushnew decision decisions :test #'eq))
	 (append (subseq (first contradictions) 0
		       (1- (length (first contradictions))))
		 (list
		  (car
		   (num:find-min-max #'(lambda (a b) (> (cdr a) (cdr b)))
				     (misc:shuffle
				      (loop for decision in decisions collect
					   (cons decision (count decision all-decisions)))))))))))

(defun most-frequent-class (table &optional (class-position (1- (length (first table)))))
  (flet ((class-in-row (row)
	   (elt row class-position)))
    (let ((all-classes      '())
	  (all-observations (loop for row in table collect (class-in-row row))))
      (loop for row in table collect
	   (pushnew (class-in-row row) all-classes :test #'eq))
      (let ((max (num:find-min-max #'(lambda (a b) (> (cdr a) (cdr b)))
				   (loop for class in all-classes collect
					(cons class (count class all-observations))))))
	(values (car max)      ; the class
		(cdr max)))))) ; the frequency

(defun make-tree (table attributes)
  (let ((classes (classes-list table)))
    (cond
      ((null table)           ; no facts, this is a problem :-(
       (error "empty examples table"))
      ((= (length classes) 1) ; all facts gives the same value, good
       (make-instance 'decision-tree
		      :data            (alexandria:lastcar (first table))
		      :decisions-count (list (cons (alexandria:lastcar (first table))
						   0))))
      ((= (length attributes) 1) ; can't go further choose the most frequent value
       (let ((most-frequent-class (most-frequent-class table)))
	 (make-instance 'decision-tree
			:data            most-frequent-class
			:decisions-count (list (cons most-frequent-class
						     0)))))
      (t                         ; split
       (let* ((gains          (loop for i from 0 below (1- (length attributes)) collect
				   (cons (nth i attributes)
					 (gain-ratio table attributes (nth i attributes)))))
	      (max-gain       (num:find-min-max #'(lambda (a b) (> (cdr a) (cdr b))) gains))
	      (new-attributes (remove (car max-gain) attributes :test #'string=))
	      (splitted       (split-by-attribute-value table attributes (car max-gain))))
	 (let* ((children (loop for i in splitted collect
			       (make-tree (if (not (numberp (elt (first (first splitted))
								 (position (car max-gain)
									   attributes))))
					       (loop for row in i collect
						    (misc:delete@ row
								  (position (car max-gain)
									    attributes)))
					       i)
					   (if (not (numberp (elt (first (first splitted))
								 (position (car max-gain)
									   attributes))))
					       new-attributes
					       attributes))))
		(path (ecase-attribute (table attributes (car max-gain))
			(symbol
			 (loop for i in splitted collect
			      (get-test-results i attributes (car max-gain))))
			(number
			 (multiple-value-bind (tables max-gain treshold)
			     (split-by-continuous-value table
							attributes
							(car max-gain))
			   (declare (ignore tables max-gain))
			   (list treshold treshold)))))
		(new-tree (make-instance 'decision-tree
					 :data            (car max-gain)
					 :path            path
					 :decisions-count (loop for i in classes collect
							       (cons i 0)))))
	   (loop for i in children do
		(add-child new-tree i))
	   new-tree))))))

(defun pessimistic-prune-branch (unpruned-tree test-table attributes
				 &optional (number-of-stddev .5))
  (let* ((pruned-tree (clone unpruned-tree)))
     (bottom-up-visit pruned-tree
 		     #'(lambda (node)
 			 (when (not (leafp node))
			   (reset-all-decisions-count pruned-tree)
			   (reset-all-decisions-count unpruned-tree)
			   (pachinko unpruned-tree test-table attributes)
			   (pachinko pruned-tree test-table attributes)
			   (let ((saved (clone node)))
			     (prune-node node)
			     (pachinko pruned-tree test-table attributes)
			     (let ((error-pruned-tree   (count-misclassified node)))
			       (multiple-value-bind (error-unprunes stdev)
				   (statistical-error saved)
				 (if (< (+ error-pruned-tree 1/2)
					(+ error-unprunes (* number-of-stddev stdev)))
				     (setf unpruned-tree (clone pruned-tree))
				     (progn
				       (setf (parent node) (parent saved)
					     (path node) (path saved)
					     (children node) (children saved)
					     (decisions-count node) (decisions-count saved)
					     (data node) (data saved))))))))))
     pruned-tree))

(defun build-tree (training-set attributes)
  "training set is a table of facts:

    '((a b 0.9 false n)
      (a c 0.1 true n)
      ....))
   where the last column is the decision.
   attributes is a list of label for each column of the table
   '(attr1 attr2...decision"
  (let* ((shuffled-training-table (shuffle-table training-set)))
    (make-tree shuffled-training-table attributes)))
