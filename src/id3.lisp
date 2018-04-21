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

(in-package :id3)

(define-constant +min-cardinality-partition+   2                                 :test #'=)

(define-constant +default-confidence-pruning+  0.25                              :test #'=)

(define-constant +epsilon+                     1.e-3                             :test #'=)

(define-constant +confidence+   #(0.0 0.001 0.005 0.01 0.05 0.10 0.20 0.40 1.0)  :test #'equalp)

(define-constant +standard-dev+ #(100.0 3.09 2.58 2.33 1.65 1.28 0.84 0.25 0.0)  :test #'equalp)

(defun gain-ratio-actual-value (info gain min-gain)
  (if (and (>= gain (- min-gain +epsilon+))
           (>  info +epsilon+))
      (/ gain info)
      (- +epsilon+)))

(defun gain-actual-value (info gain)
  (if (and (> info 0.0)
           (> gain (- +epsilon+)))
      gain
      (- +epsilon+)))

(defun calc-prob-err-coeff (confidence)
  (let ((right-limit (position-if #'(lambda (a) (<= confidence a)) +confidence+)))
    (assert (and (not (null right-limit))
                 (> right-limit 0)))
    (let* ((left-limit       (1- right-limit))
           (left-dev         (elt +standard-dev+ left-limit))
           (right-dev        (elt +standard-dev+ right-limit))
           (left-confidence  (elt +confidence+ left-limit))
           (right-confidence (elt +confidence+ right-limit))
           (dev-diff         (- right-dev left-dev))
           (conf-diff        (- right-confidence left-confidence)))
      (expt (+ left-dev
               (* dev-diff
                  (/ (- confidence left-confidence)
                     conf-diff)))
            2)))) ;; <- exponent

(defun probab-error (samples errors
                     &optional
                       (confidence  +default-confidence-pruning+)
                       (coefficient (calc-prob-err-coeff confidence)))
  (flet ((first-pass ()
           (* samples (- 1 (exp (/ (log confidence)
                                   samples))))))
    (let ((num:*default-epsilon* 1e-6))
      (cond
        ((num:epsilon= samples 0.0)
         0.0)
        ((< errors num:*default-epsilon*)
         (first-pass))
        ((< errors 0.999)
         (+ (first-pass)
            (probab-error samples 1.0 confidence coefficient)))
        ((>= (+ errors 0.5) samples)
         (* 0.67 (- samples errors)))
        (t
         (let* ((err+0.5 (+ errors 0.5))
                (pol     (+ err+0.5
                            (/ coefficient 2.0)
                            (sqrt (* coefficient
                                     (+ (* err+0.5
                                           (- 1 (/ err+0.5
                                                   samples)))
                                        (/ coefficient 4.0)))))))
           (- (* samples (/ pol
                            (+ samples coefficient)))
              errors)))))))

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
  (num:shellsort table #'< :key #'(lambda (a) (nth position a))))

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
  (multiple-value-bind (maximum-class maximum-class-count)
      (most-frequent-class table)
    (declare (ignore maximum-class))
    (let ((sorted-table  (sort-table-by-attribute (copy-tree table) attributes attribute))
          (splitter-row  1)
          (max-gain      (- +epsilon+))
          (minimum-split (max (* 0.1 (/ (table-height table) maximum-class-count))
                              +min-cardinality-partition+))
          (tries         0))
      (loop for i from 1 below (table-height sorted-table) do
           (let* ((row              (elt sorted-table i))
                  (prec-row         (elt sorted-table (1- i)))
                  (attribute-column (position attribute attributes)))
             (when (not (num:epsilon= (elt row attribute-column)
                                      (elt prec-row attribute-column)))
               (incf tries)
               (multiple-value-bind (t1 t2)
                   (split-table-by-row-position sorted-table i)
                 (let ((gain (information-gain-splitted sorted-table
                                                        attributes attribute decision
                                                        (list t1 t2))))
                   (when (and (>= (table-height t1) minimum-split)
                              (> gain max-gain))
                     (setf max-gain gain
                           splitter-row i)))))))
      (multiple-value-bind (t1 t2)
          (split-table-by-row-position sorted-table splitter-row)
        (let* ((position (position attribute attributes))
               (min-value (nth position (alexandria:lastcar t1))))
          (values (list t1 t2) max-gain min-value tries))))))

(defun split-by-attribute-position-old (table position)
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

(defun split-by-attribute-position (table position)
  (let ((all-values (all-values-set table position))
        (tables      nil))
    (loop for i in all-values do
         (push (remove-if-not #'(lambda (row)
                                  (eql i (nth position row)))
                              table)
               tables))
    (reverse tables)))

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

(defun gain-ratio (table attributes attribute average-gain
                         &key (decision (alexandria:lastcar attributes)))
  (gain-ratio-splitted table
                       attributes
                       attribute
                       decision
                       average-gain
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

(defun safe-log2 (n)
  (if (<= n 0.0)
      0.0
      (log n 2)))

(defun attribute-number-p (table column-position)
  (numberp (elt (first table) column-position)))

(defun average-gain (gains max-items attributes-matrix)
  "gains = (list (attribute . value))"
  (let ((count 0)
        (sum   0))
    (loop for gain in gains do
         (let* ((gain-value      (cdr gain))
                (attribute-name  (car gain))
                (attribute-count (length (cadr (assoc attribute-name
                                                     attributes-matrix
                                                     :test #'string=)))))
           (when (and (> gain-value
                         (- +epsilon+))
                      (< attribute-count
                         (* 0.3 max-items)))
             (incf count)
             (incf sum gain-value))))
    (if (> count 0)
        (/ sum count)
        1e6)))

(defun information-gain-splitted (original-table attributes attribute decision tables)
  (let* ((whole-entropy    (entropy original-table attributes decision))
         (position         (position attribute attributes))
         (sorted-table     (if (attribute-number-p original-table position)
                               (sort-table-by-attribute original-table attributes attribute)
                               original-table))
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
                                                    (cdr (nth i frequencies))))))
         (gain-correction (if (numberp (elt (first original-table) position))
                              (let ((tries 0))
                                (loop for i from 1 below (length sorted-table) do
                                     (let* ((row      (elt sorted-table i))
                                            (prec-row (elt sorted-table (1- i)))
                                            (attribute-column (position attribute attributes)))
                                       (when (not (num:epsilon= (elt row attribute-column)
                                                                (elt prec-row attribute-column)))
                                         (incf tries))))
                                (/ (safe-log2 tries)
                                   (table-height original-table)))
                              0.0)))

    (if (< count-card-ok 2)
        (- +epsilon+)
        (- gain gain-correction))))

(defun gain-ratio-splitted (original-table attributes attribute decision average-gain tables)
  (let* ((split-info (split-info original-table tables))
         (gain       (information-gain-splitted original-table
                                                attributes
                                                attribute
                                                decision
                                                tables)))
    (gain-ratio-actual-value split-info gain average-gain)))

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
    :accessor decisions-count
    :documentation "((class-1 . count-1) (class-2 .count-2) ... (class-n . count-n))")
   (test-table
    :initform '()
    :initarg :test-table
    :accessor test-table
    :type list)
   (associated-error
    :initform 0.0
    :initarg :associated-error
    :writer (setf associated-error))))

(defmethod marshal:class-persistant-slots ((object decision-tree))
  (append '(path)
          (call-next-method)))

(defmethod serialize ((object decision-tree))
  (format nil "~s" (marshal:marshal object)))

(defmethod deserialize ((object decision-tree) file)
  (declare (ignore object))
  (marshal:unmarshal (read-from-string (filesystem-utils:slurp-file file))))

(defgeneric associated-error (object))

(defgeneric sum-bag (object))

(defgeneric max-bag-branch (object))

(defgeneric max-bag (object))

(defgeneric test-example (object attributes example))

(defgeneric pachinko (object examples attributes &key incremental))

(defgeneric statistical-error (object))

(defgeneric count-misclassified (object))

(defgeneric count-classified (object))

(defgeneric update-decisions-count (object decision))

(defgeneric reset-all-decisions-count (object))

(defgeneric get-children-decisions-count (object))

(defgeneric prune-node-to-best-leaf (object))

(defgeneric pruning-error-leaf (object table))

(defgeneric prune-node-to-best-branch (object best-branch))

(defgeneric all-classes (object))

(defgeneric class-distribution (object))

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

(define-constant +data-el+         "data"            :test #'string=)

(define-constant +path-el+         "path"            :test #'string=)

(define-constant +decisions-count+ "decisions-count" :test #'string=)

(define-constant +errors+          "errors"          :test #'string=)

(define-constant +test-table+      "test-table"      :test #'string=)

(defmethod to-sexp ((object decision-tree))
  (let ((serialized (list nil)))
    (labels ((%serialize (decisions &optional (tree nil))
               (nappend-child tree (list
                                    (alexandria:make-keyword +data-el+) (data decisions)
                                    (alexandria:make-keyword +path-el+) (path decisions)
                                    (alexandria:make-keyword +decisions-count+)
                                    (decisions-count decisions)
                                    (alexandria:make-keyword +errors+)
                                    (associated-error decisions)))
               (map nil #'(lambda (c) (%serialize c (alexandria:lastcar tree)))
                     (children decisions))))
      (%serialize object serialized)
      (second serialized))))

(defmethod from-sexp ((object decision-tree) sexp)
  (labels ((dfs (node decision-node)
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

(defmethod associated-error ((object decision-tree))
  (if (leafp object)
      (slot-value object 'associated-error)
      (let ((res 0.0))
        (loop for c across (children object) do
             (incf res (associated-error c)))
        res)))

(defmethod sum-bag ((object decision-tree))
  (let ((res 0.0))
    (top-down-visit object
                    #'(lambda (node)
                        (incf res (reduce #'+ (mapcar #'cdr (decisions-count node))))))
    res))

(defmethod max-bag-branch ((object decision-tree))
  (let* ((all-bags   (loop for c across (children object) collect
                          (sum-bag c)))
         (max-bag    (loop for c in all-bags maximize c)))
    (values (elt (children object)
                 (position max-bag all-bags :test #'num:epsilon=))
            max-bag)))

(defmethod max-bag ((object decision-tree))
  (let* ((all-bags   (loop for c across (children object) collect
                          (sum-bag c)))
         (max-bag    (loop for c in all-bags maximize c)))
    (values (elt (children object)
                 (position max-bag all-bags :test #'num:epsilon=))
            max-bag)))

(defmethod test-example ((object decision-tree) attributes example)
  (labels((%test-example (node attributes example)
            (if (leafp node)
                (let ((class (alexandria:lastcar example)))
                  (update-decisions-count node class)
                  (data node))
                (let* ((res (example-get-by-attribute-value example attributes (data node)))
                       (res-path (if (numberp res)
                                     (if (<= res (first (path node)))
                                         0
                                         1)
                                     (position res (path node)))))
                  (%test-example (elt (children node) res-path) attributes example)))))
    (%test-example object attributes example)))

(defun take-decision (tree attributes facts)
  (test-example tree attributes facts))

(defmethod pachinko ((object decision-tree) examples attributes &key (incremental nil))
  (when (not incremental)
    (reset-all-decisions-count object))
  (mapcar #'(lambda (example)
              (test-example object attributes example))
          examples))

(defmethod statistical-error ((object decision-tree))
  (if (leafp object)
       (let* ((facts-count         (count-classified    object))
              (misclassified-count (count-misclassified object)))
         (* facts-count (probab-error facts-count misclassified-count)))
       (loop for subtree across (children object) sum
            (let* ((facts-count         (count-classified    subtree))
                   (misclassified-count (count-misclassified subtree)))
              (* facts-count (probab-error facts-count misclassified-count))))))

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
                                                     :data           (data node-from)
                                                     :test-table
                                                     (copy-tree (test-table node-from))
                                                     :associated-error (slot-value node-from
                                                                                 'associated-error)
                                                     :path           (copy-list (path node-from))
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

(defmethod prune-node-to-best-leaf ((object decision-tree))
  (let ((all-decisions-count (num:shellsort (get-children-decisions-count object)
                                            #'> :key #'cdr)))
    (setf (data object) (car (first all-decisions-count)))
    (setf (children object) #())
    (setf (path object) nil)
    object))

(defmethod prune-node-to-best-branch ((object decision-tree) best-branch)
    (setf (data object)     (data     best-branch))
    (setf (children object) (children best-branch))
    (setf (path object)     (path     best-branch))
    object)

(defmethod all-classes ((object decision-tree))
  (let ((res '()))
    (top-down-visit object
                    #'(lambda (node)
                        (map nil
                             #'(lambda (a) (pushnew (car a) res))
                             (decisions-count node))))
    res))

(defmethod class-distribution ((object decision-tree))
  (let ((all-classes (loop for c in (all-classes object) collect
                          (cons c 0.0))))
    (top-down-visit object
                    #'(lambda (node)
                        (map nil
                             #'(lambda (a)
                                 (let ((class-name  (car a))
                                       (class-count (cdr a)))
                                   (incf (cdr (assoc class-name all-classes))
                                         class-count)))
                             (decisions-count node))))
    all-classes))

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

(defun decisions-classes-set (table &optional (class-position (1- (length (first table)))))
  (all-values-set table class-position))

(defun all-decisions-classes (table &optional (class-position (1- (length (first table)))))
  (flet ((class-in-row (row)
           (elt row class-position)))
    (loop for row in table collect (class-in-row row))))

(defun most-frequent-class (table &optional (class-position (1- (length (first table)))))
    (let* ((all-classes      (decisions-classes-set table class-position))
           (all-observations (all-decisions-classes table class-position))
           (max (trivial-max (loop for class in all-classes collect
                                  (cons class (count class all-observations)))
                             :key #'cdr)))
      (values (car max)      ; the class
              (cdr max)))) ; the frequency

(defun trivial-max (l &key (key #'identity) (start-value nil))
  (let ((max         (or start-value (elt l 0)))
        (actual-list (if start-value
                         l
                         (rest l))))
    (loop for i in actual-list do
         (when (> (funcall key i)
                  (funcall key max))
           (setf max i)))
    max))

(defun make-tree (table attributes
                  &key
                    (max-items         (table-height table))
                    (all-classes       (decisions-classes-set table))
                    (attributes-matrix (build-attribute-values-matrix table attributes)))
  (let ((classes (classes-list table)))
    (cond
      ((null table)           ; no facts, this is a problem :-(
       (error "empty examples table"))
      ((= (length classes) 1) ; all facts gives the same value, good
       (make-instance 'decision-tree
                      :test-table      (copy-tree table)
                      :data            (alexandria:lastcar (first table))
                      :decisions-count (list (cons (alexandria:lastcar (first table))
                                                   0))))
      ((or (< (table-height table)
              (* 2 +min-cardinality-partition+))
           (= (length attributes) 1)) ; can't go further, choose the most frequent value
       (let ((most-frequent-class (most-frequent-class table)))
         (make-instance 'decision-tree
                        :test-table      (copy-tree table)
                        :data            most-frequent-class
                        :decisions-count (list (cons most-frequent-class
                                                     0)))))
      (t ; split
       (let* ((raw-gains      (loop for i from 0 below (1- (length attributes)) collect
                                   (cons (elt attributes i)
                                         (information-gain table attributes (nth i attributes)))))
              (average-gain   (average-gain raw-gains max-items attributes-matrix))
              (gain-ratios    (loop for i from 0 below (1- (length attributes)) collect
                                   (cons (elt attributes i)
                                         (gain-ratio table
                                                     attributes
                                                     (nth i attributes)
                                                     average-gain))))
              (max-gain-ratio (trivial-max gain-ratios
                                           :key #'cdr
                                           :start-value (cons :dummy (- +epsilon+))))
              (splitted       (split-by-attribute-value table attributes (car max-gain-ratio))))
         (multiple-value-bind (most-frequent-class most-frequent-class-count)
             (most-frequent-class table)
           (let* ((children (loop for subtable in splitted collect
                                 (make-tree subtable
                                            attributes
                                            :max-items         max-items
                                            :all-classes       all-classes
                                            :attributes-matrix attributes-matrix)))
                  (best-attribute         (car max-gain-ratio))
                  (best-attribute-outcome (cadr (assoc best-attribute attributes-matrix)))
                  (path (ecase-attribute (table attributes (car max-gain-ratio))
                          (symbol
                           (loop for i in splitted collect
                                (get-test-results i attributes (car max-gain-ratio))))
                          (number
                           (multiple-value-bind (tables max-gain-ratio treshold)
                               (split-by-continuous-value table
                                                          attributes
                                                          (car max-gain-ratio))
                             (declare (ignore tables max-gain-ratio))
                             (list treshold treshold)))))
                  (extra-paths (loop
                                  for i in best-attribute-outcome
                                  when (and i (symbolp i) (not (find i path)))
                                  collect i))
                  (extra-childen (loop for i in extra-paths collect
                                      (make-instance 'decision-tree
                                                     :data  most-frequent-class
                                                     :decisions-count
                                                     (list (cons most-frequent-class 0)))))
                  (new-tree (make-instance 'decision-tree
                                           :test-table      (copy-tree table)
                                           :data            (car max-gain-ratio)
                                           :path            (append path extra-paths)
                                           :decisions-count (loop for i in classes collect
                                                                 (cons i 0)))))
             (loop for i in (append children extra-childen) do
                  (add-child new-tree i))
             ;; check for collapse
             (pachinko new-tree table attributes :incremental nil)
             (let ((all-facts (table-height table)))
               (if (>= (count-misclassified new-tree)
                       (- all-facts most-frequent-class-count +epsilon+))
                   (progn
                     (reset-all-decisions-count new-tree)
                     (make-instance 'decision-tree
                                    :test-table      (copy-tree table)
                                    :data            most-frequent-class
                                    :decisions-count (list (cons most-frequent-class 0))))
                   new-tree)))))))))

(defun split-by-last-column (table)
  (split-by-attribute-position table
                               (1- (length (elt table 0)))))

(defmethod pruning-error-leaf ((object decision-tree) table)
  (top-down-visit object
                  #'(lambda (node)
                      (when (leafp node)
                        (setf (associated-error node) 0.0))))
  (top-down-visit object
                   #'(lambda (node)
                       (if (leafp node)
                           (let* ((best-leaf-class (trivial-max (decisions-count node)
                                                                    :key #'cdr))
                                  (cases           (reduce #'+ (decisions-count node)
                                                           :key #'cdr))
                                  (leaf-error      (- cases (cdr best-leaf-class)))
                                  (total-error     (+ leaf-error (probab-error cases
                                                                               leaf-error))))
                             (setf (associated-error node) total-error)))))
  object)

(defun all-leaf-errors (node)
  (let* ((local-class-distribution (class-distribution node))
         (max-bag (trivial-max local-class-distribution
                               :key #'cdr))
         (cases             (sum-bag  node))
         (leaf-errors       (- cases (cdr max-bag)))
         (extra-leaf-errors (probab-error cases leaf-errors)))
    (values leaf-errors extra-leaf-errors)))

(defun best-branch (node table attributes)
  (let ((cloned-node (clone node)))
    (pachinko cloned-node table attributes)
    (pruning-error-leaf cloned-node table)
    cloned-node))

(defun adjust-leafs-to-best-class (tree)
  (top-down-visit tree
                  #'(lambda (node)
                      (when (leafp node)
                        (let* ((local-class-distribution (class-distribution node))
                               (max-bag (trivial-max local-class-distribution
                                                     :key #'cdr)))
                          (when max-bag
                            (setf (data node) (car max-bag))))))))

(defun pessimistic-prune-branch (unpruned-tree test-table attributes &optional (modifiedp t))
  (if modifiedp
      (let* ((pruned-tree (clone unpruned-tree)))
        (setf modifiedp nil)
        (pachinko pruned-tree test-table attributes)
        (pruning-error-leaf pruned-tree test-table)
        (bottom-up-visit pruned-tree
                         #'(lambda (node)
                             (when (and (not modifiedp)
                                        (not (leafp node)))
                               (multiple-value-bind (leaf-errors extra-leaf-errors)
                                   (all-leaf-errors node)
                                 (let* ((max-branch    (max-bag-branch node))
                                        (tree-error    (associated-error node))
                                        (best-branch   (best-branch max-branch
                                                                    (test-table node)
                                                                    attributes))
                                        (branch-errors (associated-error best-branch)))
                                   (cond
                                     ((and (<= (+ leaf-errors extra-leaf-errors)
                                               (+ branch-errors 0.1))
                                           (<= (+ leaf-errors extra-leaf-errors)
                                               (+ tree-error 0.1)))
                                      (format t "pruning node ~a~%" node)
                                      (prune-node-to-best-leaf node)
                                      (format t "-------->~%~a~%" node)
                                      (setf modifiedp t))
                                     ((and max-branch
                                           (<= branch-errors
                                               (+ tree-error 0.1)))
                                      (setf modifiedp t)
                                      (format t "pruning branch ~a~%" node)
                                      (prune-node-to-best-branch node best-branch)
                                      (adjust-leafs-to-best-class node)
                                      (format t "-------->~%~a~%" max-branch))
                                     (t
                                      (format t "NO pruning~%"))))))))
        (pessimistic-prune-branch pruned-tree test-table attributes modifiedp))
      unpruned-tree))

(defun build-attribute-values-matrix (table attributes)
  (loop for i from 0 below (length attributes) collect
       (let ((tail '()))
         (loop for row in table when (symbolp (elt row i)) do
              (pushnew (elt row i) tail))
         (cons (elt attributes i)
               (list tail)))))

(defun build-tree (training-set attributes &key (prune t))
  "training set is a table of facts:

    '((a b 0.9 false n)
      (a c 0.1 true n)
      ....))
   where the last column is the decision.
   attributes is a list of label for each column of the table
   '(attr1 attr2...decision)"
  (let* ((shuffled-training-table (shuffle-table training-set))
         (unpruned-tree           (make-tree shuffled-training-table attributes)))
    (if prune
        (pessimistic-prune-branch unpruned-tree
                                  shuffled-training-table
                                  attributes)
        unpruned-tree)))
