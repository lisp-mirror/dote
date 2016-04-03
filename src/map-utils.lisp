(in-package :map-utils)

(defun facingp (pos dir target-pos &key (max-distance 1))
  "dir is a 3d vector"
  (let* ((dir-to-target (ivec2- target-pos pos))
	 (dot-product   (vec2-dot-product (vec2-normalize (vec2 (d (elt dir-to-target 0))
								(d (elt dir-to-target 1))))
					       (vec2 (d (elt dir 0))
						     (d (elt dir 2))))))
    (and (f<= (ivec2-length dir-to-target)
	      max-distance)
	 (epsilon= dot-product 1.0))))

(defun pos-entity-chunk->cost-pos (pos)
  (ivec2:ivec2 (coord-chunk->costs (elt pos 0))
	       (coord-chunk->costs (elt pos 2))))
