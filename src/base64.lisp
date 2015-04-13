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

(in-package :base64)

(alexandria:define-constant +encode-table+ 
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/" :test #'equalp)

(alexandria:define-constant +padding-char+ #\= :test #'equalp)

(eval-when (:compile-toplevel :load-toplevel :execute)  
  (defun gen-encode-table ()
    (let ((res (make-hash-table)))
      (loop 
	 for v across +encode-table+ 
	 for i from 0 by 1          do
	   (setf (gethash v res) i))
      (setf (gethash +padding-char+ res) 0) ;; padding
      res))

  (alexandria:define-constant +decode-table+ (gen-encode-table) :test #'equalp))

(defun encode (data)
  (declare (optimize (debug 0) (safety 0) (speed 1)))
  (declare (vector data))
  (let* ((len (length data))
	 (padding (if (< len 3) (- 3 len) (if (= 0 (mod len 3)) 0 (- 3 (mod len 3)))))
	 (padding-string (misc:make-array-frame padding +padding-char+ 'character t))
	 (actual-data (concatenate 'vector data (misc:make-array-frame padding #x0 
								       'fixnum t)))
	 (res (misc:make-array-frame (+ (length actual-data)
					(floor  (/ (length actual-data) 3)))
				     #\A 'character t)))
    (declare (fixnum padding))
    (declare (vector res))
    (declare (simple-array actual-data padding-string))
    (declare (simple-string res))
    (loop 
       for i from 0 below len by 3 
       for ct from 0 by 4 do
	 (let* ((a  (elt actual-data i))
		(b  (elt actual-data (+ i 1)))
		(c  (elt actual-data (+ i 2)))
		(e1 (ldb (byte 6 2) a))
		(e2 (logior (ash (ldb (byte 2 0) a) 4)
			    (ldb (byte 4 4) b)))
		(e3 (logior (ash (ldb (byte 4 0) b) 2)
			    (ldb (byte 2 6) c)))
		(e4 (ldb (byte 6 0) c)))
	   (declare ((unsigned-byte 8) a b c e1 e2 e3 e4))
	   (setf (elt res ct) (elt +encode-table+ e1))
	   (setf (elt res (+ ct 1))  (elt +encode-table+ e2))
	   (setf (elt res (+ ct 2))  (elt +encode-table+ e3))
	   (setf (elt res (+ ct 3))  (elt +encode-table+ e4))))
    (setf (subseq res (- (length res) padding) (length res))
	  padding-string)
    res))

(defun decode (s)
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (declare (simple-string s))
  (let ((padding (count-if #'(lambda (a) (char= +padding-char+ a))
			   (subseq s (- (length s) 3))))
	(res     (misc:make-array-frame 0 t t nil))
	(input   (text-utils:strip-withespaces s)))
    (declare (fixnum padding))
    (declare (vector res))
    (declare (simple-string input))
    (loop for i from 0 below (length s) by 4 do
	 (let* ((a  (gethash (elt input i) +decode-table+))
		(b  (gethash (elt input (+ i 1)) +decode-table+))
		(c  (gethash (elt input (+ i 2)) +decode-table+))
		(d  (gethash (elt input (+ i 3)) +decode-table+))
		(d1 (logior (ash a 2)
			    (ldb (byte 2 4) b)))
		(d2 (logior (ash (ldb (byte 4 0) b) 4)
			    (ldb (byte 4 2) c)))
		(d3 (logior (ash (ldb (byte 2 0) c) 6) d)))
	   (declare ((unsigned-byte 8) a b c d d1 d2 d3))
	   (vector-push-extend d1 res)
	   (vector-push-extend d2 res)
	   (vector-push-extend d3 res)))
    (subseq res 0 (- (length res) padding))))
