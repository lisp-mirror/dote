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

(in-package :rb-tree-test)

(defsuite rb-tree-suite (all-suite))

(defun %compare-fn (a b) (< (car a) (car b)))

(defun make-test-tree (&optional (ct 100))
  (let ((time (get-universal-time))
	(key-fn     #'car)
	(equal-fn   #'=)
	(compare-fn #'<)
	(tree       (make-root-rb-node nil +rb-red+)))
    (num:with-lcg-seed (time)
      (loop repeat ct do
	 (setf tree (rb-tree:insert tree
				    (cons (num:lcg-next-upto ct) nil)
				    :equal     equal-fn
				    :compare   compare-fn
				    :key-datum key-fn
				    :key       key-fn)))
      tree)))

(deftest test-insert (rb-tree-suite)
  (assert-true (balancedp (make-test-tree 10000))))

(defmacro %test-remove ()
  `(progn
     ,@(loop repeat 1000 collect
	    `(assert-true
		 (progn
		   (sleep 1)
		   (let* ((count         1000)
			  (tree          (make-test-tree count))
			  (key-datum-fn  #'identity)
			  (key-fn        #'car)
			  (equal-fn      #'=)
			  (cmp-fn        #'%compare-fn))
		     (num:with-lcg-seed ((get-universal-time))
		       (every #'(lambda (a) a)
			      (loop repeat (floor (/ count 5)) collect
				   (let ((datum (num:lcg-next-upto count))
					 (balanced-before-remove-p (bstp tree
									 :comp-fn cmp-fn)))
				     (setf tree (remove-node tree
							     datum
							     :equal     equal-fn
							     :compare   #'<
							     :key-datum key-datum-fn
							     :key       key-fn))
				     (and balanced-before-remove-p
					  (bstp tree :comp-fn cmp-fn)
					  (balancedp tree)
					  (not (search tree
						       datum
						       :equal     equal-fn
						       :compare   #'<
						       :key-datum key-datum-fn
						       :key       key-fn)))))))))))))

(deftest test-remove (rb-tree-suite)
  (%test-remove))
