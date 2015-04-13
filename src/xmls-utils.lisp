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

(in-package xmls-utils)

(defmacro with-tagmatch ((tag node) &body body)
  `(if (xmls:xmlrep-tagmatch ,tag ,node)
       (progn ,@body)
       (error 'xml-no-matching-tag 
	      :text (format nil 
			    "Error in parsing scheme database, expecting ~s got ~s instead." ,tag 
			    (xmls:xmlrep-tag ,node)))))


(defmacro with-tagmatch-if-else ((tag node else) &body body-then)
  `(if (xmls:xmlrep-tagmatch ,tag ,node)
       (progn ,@body-then)
       (progn ,@else)))



(defmacro with-attribute ((att node) &body body)
  (alexandria:with-gensyms (value)
    `(let  ((,value (xmls:xmlrep-attrib-value ,att ,node nil)))
       (if (string/= ,value nil)
	   (progn 
	     ,@body
	     ,value)
	   (error 'xml-no-such-attribute 
		  :text (format nil 
				"Error in parsing scheme database, no attribute ~s found in node ~a."				
				,att
				(quote ,node)))))))
