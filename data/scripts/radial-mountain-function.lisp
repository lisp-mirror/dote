;; just a placeholder

(in-package :level-config)

(defun radial-mountain-z-height-function (&optional
					    (sigma-function 
					     #'(lambda (map-size) (/ map-size 4)))
					    (minimum-z +zero-height+)
					    (maximum-z +maximum-mountain-height+))
  #'(lambda (map x y iteration)
      (declare (ignore iteration))
      (let* ((map-maximum-linear-size (max (matrix:width  (matrix map))
					   (matrix:height (matrix map))))
	     (center (2d-utils:center-aabb2 (aabb map)))
	     (dist (2d-utils:2d-vector-magn 
		    (2d-utils:2d-vector-diff center (list x y))))
	     (gaussian-function (gaussian-function maximum-z 
						   (funcall 
						    sigma-function 
						    map-maximum-linear-size)
						   0))
	     (z (alexandria:clamp
		 (abs (gaussian-probability 1 (funcall gaussian-function dist)))
		 minimum-z maximum-z)))
	z)))

