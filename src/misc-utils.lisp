;; random map generator
;; Copyright (C) 2012  cage

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

(in-package :misc-utils)

;; debug utils

(defparameter *debug* nil)

(defmacro when-debug (&body body)
  `(when (not (null *debug*))
     ,@body))

(defun debug-log (format-string &rest parameters)
  (when (not (log:debug))
    (log:config :debug :nopackage :nofile))
  (let ((message (apply #'format nil format-string parameters)))
    (log:debug message)))

(defun dbg (format-string &rest parameters)
  (apply #'debug-log format-string parameters))

(defun dump-hash-table (table)
  (let ((res '()))
    (maphash (lambda (k v) (push (format nil "~s -> ~s~%" k v) res)) table)
    res))

(defun dump-hashtable (table)
  (maphash (lambda (k v) (misc:dbg "~s -> ~s" k v)) table))

;; functions utils

(defmacro define-compiler-macros (name &body args)
  (let* ((function-name (alexandria:format-symbol t "~:@(~a~)" name))
	 (low-level-function-name (alexandria:format-symbol t "~:@(%~a~)" name)))
    `(define-compiler-macro ,function-name (&whole form ,@args)
       (let ((low-funname ',low-level-function-name))
	 (if (every #'constantp (list ,@args))
	     (progn
	       `(funcall (function ,low-funname) ,,@args))
	     (progn
	       form))))))

(defmacro definline (name arg &rest body)
  (let* ((function-name (alexandria:format-symbol t "~:@(~a~)" name)))
    `(progn
       (declaim (inline ,function-name))
       (defun ,function-name (,@arg) ,@body))))

(defmacro defun-inline-function (name arg &body body)
  (let* ((function-name (alexandria:format-symbol t "~:@(~a~)" name))
	 (low-level-function-name (alexandria:format-symbol t "~:@(%~a~)" name)))
    `(progn
       (declaim (inline ,function-name))
       (defun ,function-name (,@arg) (,low-level-function-name ,@arg))
       (defun ,low-level-function-name (,@arg) ,@body))))

(defmacro defmethod-inline-function (name arg &body body)
  (let* ((function-name (alexandria:format-symbol t "~:@(~a~)" name))
	 (low-level-function-name (alexandria:format-symbol t "~:@(%~a~)" name)))
    `(progn
       (declaim (inline ,function-name))
       (defgeneric ,low-level-function-name (,@(loop for i in arg collect
						    (if (atom i)
							i
							(first i)))))

       (defmethod ,function-name (,@arg) (,low-level-function-name
					  ,@(loop for i in arg collect
						 (if (atom i)
						     i
						     (first i)))))
       (defmethod ,low-level-function-name (,@arg) ,@body))))

(defmacro defcached (name (arg &key (test 'equalp) (clear-cache nil))
		     declaration
		     (&body body))
  (let* ((function-name (alexandria:format-symbol t "~:@(~a~)" name))
	 (cache-name (alexandria:format-symbol t "~:@(cache~)")))
    `(let ((,cache-name (make-hash-table :test (quote ,test))))
       (defun ,function-name (,@arg) ,(if declaration
					  declaration
					  `(declare (optimize (speed 0) (safety 3) (debug 3))))

	 (and ,clear-cache (setf ,cache-name (make-hash-table :test (quote ,test))))
	 ,@(list body)))))

(defun nest-expressions (data &optional (leaf nil))
  (if (null data)
      (list leaf)
      (append (first data) (if (rest data)
			       (list (nest-expressions (rest data) leaf))
			       (nest-expressions (rest data) leaf)))))

(defun replace-e! (expr num)
  (if (null (first expr))
      nil
      (if (atom (first expr))
	  (append (list
		   (if (eq (first expr) :e!)
		       num
		       (first expr)))
		  (replace-e! (rest expr) num))
	  (append (list (replace-e! (first expr) num))
		  (replace-e! (rest expr) num)))))

(alexandria:define-constant +nil-equiv-bag+ '(:none :false :nil) :test #'equalp)

(defun build-plist (params)
  (let ((keywords (mapcar #'alexandria:make-keyword
			  (loop for i from 0 below (length params) when (oddp (1+ i))
			     collect (elt params i))))
	(vals (mapcar #'(lambda (a)
			  (typecase a
			    (symbol (let ((key (alexandria:make-keyword a)))
				      (and (not (find key +nil-equiv-bag+ :test #'eq))
					   key)))
			    (cons   (list a))
			    (otherwise a)))
		      (loop for i from 0 below (length params) when (evenp (1+ i))
			 collect (elt params i)))))
    (mapcar #'(lambda (a b) (cons a b)) keywords vals)))


(defmacro build-assocs-chain (path start)
  (if (null path)
      start
      `(cdr (assoc ,(first path) (build-assocs-chain ,(rest path) ,start)))))

(defmacro gen-trivial-plist-predicate (name class var get-fn)
  (let ((name-fn (alexandria:format-symbol t "~:@(~a-p~)" name)))
    `(progn
       (defgeneric ,name-fn (object))
       (defmethod  ,name-fn ((object ,class))
	 (funcall ,get-fn object ,var)))))

(defmacro gen-trivial-plist-predicates (class get-fn &rest vars)
  `(progn
     ,@(loop for v in vars collect
	    `(gen-trivial-plist-predicate ,(alexandria:symbolicate (string-trim "+" v))
					  ,class
					  ,v
					  (function ,get-fn)))))

(defmacro gen-trivial-plist-get (function-name-prefix name class var get-fn)
  (let ((name-fn (alexandria:format-symbol t "~:@(~a-~a~)" function-name-prefix name)))
    `(progn
       (defgeneric ,name-fn (object))
       (defmethod  ,name-fn ((object ,class))
	 (funcall ,get-fn object ,var)))))

(defmacro gen-trivial-plist-gets (class get-fn function-name-prefix &rest vars)
  `(progn
     ,@(loop for v in vars collect
	    `(gen-trivial-plist-get ,function-name-prefix
				    ,(alexandria:symbolicate (string-trim "+" v))
				    ,class
				    ,v
				    (function ,get-fn)))))

;; plist

(defun recursive-assoc (path start)
  (if (null path)
      start
      (recursive-assoc (rest path) (cdr (assoc (first path) start)))))

(defun recursive-assoc-just-before (path start)
  (if (= (length path) 1)
      start
      (recursive-assoc-just-before (rest path) (cdr (assoc (first path) start)))))

(defun n-setf-path-value (db path new-value)
  (let* ((ptr (recursive-assoc-just-before path db))
	 (last-key (alexandria:last-elt path))
	 (last-cons (assoc last-key ptr)))
    (if last-cons
	(values (setf (cdr last-cons) new-value) t)
	(values nil nil))))

;; misc

(defun not-null-p (a)
  (not (null a)))

(definline code->char (code &key (limit-to-ascii nil))
  (code-char (if limit-to-ascii
		 (alexandria:clamp code 0 127)
		 code)))

(definline char->code (code)
  (char-code code))

;;;; binary files utils

(defun 2byte->word (byte1 byte2) ;; little endian
  (let ((res #x00000000))
    (boole boole-ior
	   (boole boole-ior byte1 res)
	   (ash byte2 8))))

(defun 2word->int (word1 word2)
  (let ((res #x00000000))
    (boole boole-ior
	   (ash (boole boole-ior word1 res) 16)
	   word2)))

(defun byte->int (bytes)
  (let ((res #x0000000000000000))
    (loop
       for i in bytes and
       ct = 0 then (+ ct 8) do
	 (setf res
	       (boole boole-ior
		      (ash i ct)
		      res)))
    res))

(defmacro gen-intn->bytes (bits)
  (let ((function-name (alexandria:format-symbol t "~:@(int~a->bytes~)" bits)))
  `(defun ,function-name (val &optional (count 0) (res '()))
     (if (>= count ,(/ bits 8))
	 res
	 (,function-name (ash val -8) (1+ count) (push (boole boole-and val #x00ff) res))))))

(gen-intn->bytes 16)

(gen-intn->bytes 32)

(defun bytes->string (bytes)
  (coerce (mapcar #'code-char bytes) 'string))

(defun read-ieee-float-32 (stream)
  (let ((bytes (make-fresh-list 4)))
    (read-sequence bytes stream)
    (let ((bits (byte->int bytes)))
      (ieee-floats:decode-float32 bits))))

(defmacro define-offset-size (package prefix &rest name-offset-size)
   `(progn
      ,@(loop for i in name-offset-size collect
	     `(progn
		(alexandria:define-constant
		    ,(alexandria:format-symbol package "~@:(+~a-~a-offset+~)" prefix (first i))
		    ,(second i) :test #'=)
		,(when (= (length i) 3)
		       `(alexandria:define-constant
			    ,(alexandria:format-symbol package "~@:(+~a-~a-size+~)" prefix
						       (first i))
			    ,(third i) :test #'=))))))

(defmacro define-parse-header-chunk ((name offset size object &optional (slot name)))
  (alexandria:with-gensyms (bytes)
    `(progn
       (defgeneric ,(alexandria:format-symbol t "PARSE-~:@(~a~)" name) (,object stream))
       (defmethod ,(alexandria:format-symbol t "PARSE-~:@(~a~)" name) ((object ,object) stream)
	 (file-position stream ,offset)
	 (let* ((,bytes (make-fresh-list ,size)))
	   (read-sequence ,bytes stream)
	   ,(when (not (null slot))
		  `(setf (,slot object) ,bytes))
	   (values ,bytes object))))))

(defun read-list (stream size &key (offset nil))
  (when offset
    (file-position stream offset))
  (let* ((bytes (misc-utils:make-fresh-list size)))
    (read-sequence bytes stream)
    bytes))

(defun read-array (stream size &key (offset nil))
  (when offset
    (file-position stream offset))
  (let* ((bytes (misc-utils:make-array-frame size 0 '(unsigned-byte 8) t)))
    (read-sequence bytes stream)
    bytes))

;; sequence utils

(defun vector-empty-p (v)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (vector v))
  (= (length v) 0))

(defun random-num-filled-vector (size max)
  (map-into (misc:make-array-frame size max (type-of max) t)
	    #'(lambda () (num:lcg-next-upto max))))

(defmacro random-elt (seq)
  `(elt ,seq (num:lcg-next-upto (length ,seq))))

(defun make-fresh-list (size &optional (el nil))
  (map-into (make-list size)
	    (if (functionp el)
		el
		#'(lambda () el))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-array-frame (size &optional (el nil) (type t) (simplep nil))
    "All elements points to the same address/reference!"
    (make-array size
		:fill-pointer (if (not simplep) size nil)
		:adjustable (if (not simplep) t nil)
		:initial-element el
		:element-type type)))

(defun make-fresh-array (size &optional (el nil) (type t) (simplep nil))
  (let ((res (make-array size
			 :fill-pointer (if (not simplep) size nil)
			 :adjustable (if (not simplep) t nil)
			 :initial-element el
			 :element-type type)))
    (map-into res #'(lambda (a) (setf a (cond
					  ((functionp el)
					   (funcall el))
					  ((arrayp el)
					   (alexandria:copy-array el))
					  ((listp el)
					   (copy-list el))
					  (t
					   el))))
	      res)))

(defun list->array (the-list)
  (make-array (length the-list)
	      :fill-pointer (length the-list)
	      :adjustable t
	      :initial-contents (copy-list the-list)))

(defun copy-list-into-array (from to)
  (assert (= (length from) (length to)))
  (loop
     for i in from
     for ct from 0 by 1 do
       (setf (elt to ct) i))
  to)

(defun list->simple-array (the-list start-type type)
  (let ((res (make-array-frame (length the-list) start-type type t)))
    (loop
       for element in the-list
       for i from 0 below (length the-list) do
	 (setf (elt res i) element))
    res))

(defun permutation (li)
  (let ((res-partial '())
	(res '()))
    (labels ((perm (start tail)
	       (let ((partial-tree '()))
		 (loop for i in start do
		      (loop for j in (set-difference tail i) do
			   (push (append  i (list j)) partial-tree)))
		 (setf res-partial (reverse (copy-tree partial-tree))))))
      (loop for ct in li do
	   (do ((start (list (list ct)) res-partial))
	       ((null (set-difference li (first start)))
		(progn
		  (setf res (append res res-partial))
		  (setf res-partial '())))
	     (perm start li))))
    res))

(defun shuffle (seq)
  (loop for i from (1- (length seq)) downto 1 do
       (let ((swap nil)
	     (rnd (mod (num:lcg-next) i)))
	 (setf swap (elt seq rnd))
	 (setf (elt seq rnd) (elt seq i))
	 (setf (elt seq i) swap)))
  seq)

(defun split-into-sublist (lst len)
  (if (< (length lst) len)
      (if (null lst)
	  lst
	  (list lst))
      (append (list (subseq lst 0 len)) (split-into-sublist (subseq lst len) len))))

(defgeneric delete@ (sequence position))

(defgeneric safe-delete@ (sequence position)
  (:documentation "Return sequence if position is out of bound"))

(defmacro gen-delete@ ((sequence position) &body body)
  `(if (and (>= ,position 0)
	    (< ,position (length ,sequence)))
       ,@body
      (error 'conditions:out-of-bounds :seq sequence :idx position)))


(defmethod delete@ ((sequence list) position)
  (gen-delete@
   (sequence position)
   (append (subseq sequence 0 position)
	   (and (/= position (- (length sequence) 1))
		(subseq sequence (1+ position))))))

(defmethod delete@ ((sequence vector) position)
  (gen-delete@
   (sequence position)
    (make-array (1- (length sequence))
		:fill-pointer (1- (length sequence))
		:adjustable t
		:initial-contents (concatenate 'vector (subseq sequence 0 position)
					       (and (/= position (- (length sequence) 1))
						    (subseq sequence (1+ position)))))))

(defmethod safe-delete@ ((sequence sequence) position)
  (restart-case
      (delete@ sequence position)
    (return-nil () nil)
    (return-whole () sequence)
    (new-index (i) (safe-delete@ sequence i))))

(defgeneric remove-compact-remap-sequence (sequence predicate))

(defmethod remove-compact-remap-sequence ((sequence list) predicate)
  (let ((nullified (loop
		      for i in sequence
		      for ct from 0 collect
			(if (funcall predicate ct i)
			    nil
			    i)))
	(mapping nil)
	(results '()))
    (loop
       for i in nullified
       for pos from 0 do
	 (when (not (null i))
	   (push i results)
	   (push (list pos (1- (length results))) mapping)))
    (values (reverse results) mapping)))

(defmethod remove-compact-remap-sequence ((sequence vector) predicate)
  (let ((nullified (loop for i from 0 below (length sequence) collect
			(if (funcall predicate i (elt sequence i))
			    nil
			    (elt sequence i))))
	(mapping nil)
	(results (make-array-frame 0)))
    (loop for i from 0 below (length nullified) do
	 (when (not (null (elt nullified i)))
	   (vector-push-extend (elt nullified i) results)
	   (push (list i (1- (length results))) mapping)))
    (values results mapping)))

(defun remove-if-null (a)
  (remove-if #'null a))

;; iterations

(defmacro do-while (declaration return-form &body body)
  "C-like \"do { ...} while (condition)\" statement: body is evaluated
  even if exit condition is t at the very first iteration"
  (alexandria:with-gensyms (first-iteration)
    `(do ,(append (list `(,first-iteration t nil))
		  declaration)
	 ,(append (list `(if ,first-iteration
			     nil
			     ,(first return-form)))
		  (rest return-form))
       ,@body)))

(defmacro do-while* (declaration return-form &body body)
  "C-like \"do { ...} while (condition)\" statement: body is evaluated
  even if exit condition is t at the very first iteration"
  (alexandria:with-gensyms (first-iteration)
    `(do* ,(append (list `(,first-iteration t nil))
		   declaration)
	  ,(append (list `(if ,first-iteration
			      nil
			      ,(first return-form)))
		   (rest return-form))
       ,@body)))

;; maps

(defun coord-map->chunk (a &key (tile-offset (num:d/ +terrain-chunk-tile-size+ 2.0)))
  "convert from logical (i.e. matrix of integer) to actual (float) rendering coordinate"
  (num:d+ (num:d* (num:desired a) +terrain-chunk-size-scale+) tile-offset))

(defun coord-terrain->chunk (a &key (tile-offset (num:d/ +terrain-chunk-tile-size+ 2.0)))
  "convert from terrain matrix to actual rendering coordinate"
  (num:d+ (num:d* (num:desired a)
		  (num:d+ +terrain-chunk-tile-size+ +terrain-chunk-size-scale+))
	  tile-offset))

(definline coord-chunk->matrix (a)
  "convert from terrain chunk to matrix"
  (declare (optimize (speed 1) (safety 0) (debug 0)))
  (declare (num:desired-type a))
  (floor (num:d/ a +terrain-chunk-tile-size+)))

(definline coord-chunk->costs (a)
  "convert from terrain chunk to costs matrix"
  (coord-chunk->matrix (num:d a)))

;; cffi

(definline make-null-pointer ()
  (cffi:null-pointer))
