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

(in-package :parallel-utils)

(defparameter *parallel-setf-queue* (lparallel.queue:make-queue :initial-contents '(t)
                                                                :fixed-capacity 1))

(defmacro with-mutex (&body body)
  `(progn
     (lparallel.queue:pop-queue *parallel-setf-queue*)
     (prog1
         (progn ,@body)
       (lparallel.queue:push-queue t *parallel-setf-queue*))))

;; todo use with-mutex
(defmacro parallel-setf (place value)
  `(progn
     (lparallel.queue:pop-queue *parallel-setf-queue*)
     (prog1
         (setf ,place ,value)
       (lparallel.queue:push-queue t *parallel-setf-queue*))))


(defmacro with-workers ((workers-number &key (number `(if (> (os-utils:cpu-number) 1)
                                                          (os-utils:cpu-number)
                                                          1)))
                        &body body)
  `(unwind-protect
        (let ((,workers-number ,number))
          ,@body)
     (lparallel:end-kernel :wait t)))

(defmacro with-fallback (timeout fallback-fn &body body)
  (alexandria:with-gensyms (res)
    `(let* ((channel (lparallel:make-channel)))
       ,@body
       (let ((,res (lparallel:try-receive-result channel :timeout ,timeout)))
         (if ,res
             ,res
             (funcall ,fallback-fn))))))

(defmacro with-kernel (&body body)
  `(with-workers (workers-number)
     (let ((lparallel:*kernel* (lparallel:make-kernel workers-number)))
       ,@body)))

(defun build-bg-process (fn timeout &key (cleanup-fn nil))
  (let ((channel :start)
        (results nil))
    #'(lambda ()
        (cond
          ((eq channel :start)
           (setf channel (lparallel:make-channel))
           (lparallel:submit-task channel fn))
          ((null channel)
           results)
          (t
           (setf results (lparallel:try-receive-result channel :timeout timeout))
           (when results
             (setf channel nil)
             (and  (functionp cleanup-fn)
                   (funcall cleanup-fn))
             results))))))
