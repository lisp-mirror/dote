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

(in-package :kanren-utils)

(defun kanren-format-symbol-predicate (name)
  (format-symbol t
		 (if (> (count "-" (symbol-name name) :test #'string=) 0)
		     "~:@(~a-o~)"
		     "~:@(~ao~)")
		 name))

(defmacro facts (name &rest facts-list)
  (when (null facts-list)
    (error "Facts-list null"))
  (let ((arity (length (first facts-list))))
    (when (find-if #'(lambda (a) (/= arity (length a))) facts-list)
      (error "At least an element of facts-list has different size from another"))
    (let ((fn-name (kanren-format-symbol-predicate name))
	  (params  (loop for i from 0 below arity collect (gensym))))
      `(defun ,fn-name ,(if (null params)
			    '()
			    params)
	 (conde
	   ,@(loop for fact in facts-list collect
		 `(;(fresh (,out)
		    ,@(append
		       (loop
			  for i from 0 below (1- arity)
			  for var in (subseq fact 0 (1- (length fact))) collect
			    `(== ,var ,(elt params i)))
		       (if (not (null params))
			   (list `(== ,(alexandria:last-elt fact)
				      ,(alexandria:last-elt params)))
			   nil)))))))))
