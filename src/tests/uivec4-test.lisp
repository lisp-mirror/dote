(in-package :uivec4-test)

(defsuite uivec4-suite (all-suite))

(deftest creation-test (uivec4-suite)
  (assert-equalp
      (uivec4 0 1 2 3)
      #(0 1 2 3)))

(deftest copy-test (uivec4-suite)
  (let ((orig (uivec4 (num:lcg-next-upto 1000) (num:lcg-next-upto 1000)
		      (num:lcg-next-upto 1000) (num:lcg-next-upto 1000))))
    (assert-equalp
	orig
	(copy-uivec4 orig))))
