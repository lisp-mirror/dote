;; Copyright (c) 2013 Masataro Asai (guicho2.71828@gmail.com)
;; Licensed under the LLGPL License.

(in-package :num-utils)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro define-typed-op (name op type
                             &key
                               (return-value-on t)
                               (input-value-on t))
    `(defmacro ,name (&rest args)
       (let ((the-args
              ,(if input-value-on
                   `(mapcar #'(lambda (argsym)
                                `(the ,',type ,argsym))
                            args)
                   `args)))
         ,(if return-value-on
              ``(the ,',type
                     (,',op ,@the-args))
              ``(,',op ,@the-args)))))

  (set-dispatch-macro-character #\§ #\d
                                #'(lambda (stream char1 num)
                                    (declare (ignore stream char1 num))
                                    (find-class 'single-float)))

  (defmacro class-of-desired-type () `(find-class 'single-float))

  (deftype desired-type () 'single-float)

  (declaim (inline desired))

  (defun desired (arg)
    (coerce arg 'desired-type))

  (declaim (inline d))

  (defun d (a)
    (desired a)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; ops

(define-typed-op d+ + desired-type)

(define-typed-op d* * desired-type)

(define-typed-op d- - desired-type)

(define-typed-op d/ / desired-type)

(define-typed-op dsqrt sqrt desired-type)

(define-typed-op daref aref desired-type :input-value-on nil)

(define-typed-op dmin min desired-type)

(define-typed-op dmax max desired-type)

(define-typed-op d> > desired-type :return-value-on nil)

(define-typed-op d< < desired-type :return-value-on nil)

(define-typed-op d>= >= desired-type :return-value-on nil)

(define-typed-op d<= <= desired-type :return-value-on nil)

(define-typed-op d= = desired-type :return-value-on nil)

(define-typed-op drandom random desired-type)

(define-typed-op dslot-value slot-value desired-type :input-value-on nil)

(define-typed-op dsetf setf desired-type :input-value-on nil)

(define-typed-op dabs abs desired-type :return-value-on nil)

(define-typed-op dplusp plusp desired-type :return-value-on nil)

(define-typed-op dminusp minusp desired-type :return-value-on nil)

(define-typed-op dzerop zerop desired-type :return-value-on nil)

(define-typed-op dcos cos desired-type)

(define-typed-op dsin sin desired-type)

(define-typed-op dtan tan desired-type)

(define-typed-op dcosh cosh desired-type)

(define-typed-op dsinh sinh desired-type)

(define-typed-op dtanh tanh desired-type)

(define-typed-op dacos acos desired-type)

(define-typed-op dasin asin desired-type)

(define-typed-op datan atan desired-type)

(define-typed-op dexp exp desired-type)

(define-typed-op dexpt expt desired-type)

(define-typed-op dlog log desired-type)

(define-typed-op dfloor floor desired-type)

(defun dlerp (w a b)
  (declare (optimize (debug 0) (safety 0) (speed 3))
           (type desired-type w a b))
  (d+ a (d* w (d- b a))))

(defun secure-dacos (a)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (declare (desired-type a))
  (dacos (dmin 1.0 a)))

;;;; constants

(alexandria:define-constant +eps+ 1.0e-10 :test #'=)

(alexandria:define-constant +delta+ 1.0e-2 :test #'=)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (alexandria:define-constant +2pi+ (d* 2.0e0 (desired pi)) :test #'=))

;; parsing helper

(defun parse-number->desired (num)
  (desired (parse-number:parse-number num)))

(declaim (inline f))

(defun f (num)
  (coerce num 'fixnum))

(defmacro f* (&body body)
  `(the fixnum (* ,@body)))

(defmacro f/ (&body body)
  `(the fixnum (/ ,@body)))

(defmacro f+ (&body body)
  `(the fixnum (+ ,@body)))

(defmacro f- (&body body)
  `(the fixnum (- ,@body)))

(defmacro f< (&body body)
  `(the boolean (< ,@body)))

(defmacro f> (&body body)
  `(the boolean (> ,@body)))

(defmacro f<= (&body body)
  `(the boolean (<= ,@body)))

(defmacro f>= (&body body)
  `(the boolean (>= ,@body)))
