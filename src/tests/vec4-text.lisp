(in-package :vec4-test)

(defsuite vec4-suite (all-suite))

(deftest creation-test (vec4-suite)
  (assert-equality #'vec4~
      (vec4 0.0 1.0 2.0 3.0)
      #(0.0 1.0 2.0 3.0))
  (assert-false
      (vec4~ (vec4 0.0 1.0 2.0 3.0)
	     #(0.0 1.0 2.0 3.00000000000001))))
      

(deftest copy-test (vec4-suite)
  (let ((orig (uivec4 (num:lcg-next-upto 1000.0) (num:lcg-next-upto 1000.0)
		      (num:lcg-next-upto 1000.0) (num:lcg-next-upto 1000.0))))
    (assert-equality #'vec4~
	orig
	(copy-vec4 orig))))
