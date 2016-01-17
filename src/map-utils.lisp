(in-package :map-utils)

(defun facingp (pos dir target-pos)
  "dir is a 3d vector"
  (let* ((dir-to-target (ivec2:ivec2- target-pos pos))
	 (dot-product   (vec2:vec2-dot-product (vec2:vec2 (num:d (elt dir-to-target 0))
							  (num:d (elt dir-to-target 1)))
					       (vec2:vec2 (num:d (elt dir 0))
							  (num:d (elt dir 2))))))
    (num:epsilon= dot-product 1.0)))

(defun pos-entity-chunk->cost-pos (pos)
  (ivec2:ivec2 (coord-chunk->costs (elt pos 0))
	       (coord-chunk->costs (elt pos 2))))
