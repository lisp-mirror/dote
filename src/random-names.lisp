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

(in-package :random-names)

(alexandria:define-constant +vowels+ "[aeiou]" :test #'string=)

(alexandria:define-constant +consonants+ "[bcdfghlmnpqrstvwxyz ']" :test #'string=)

(alexandria:define-constant +starter+ "<" :test #'string=)

;; word  := (wovels consonants vowels)*
;; wovels :=  [aeiou]+
;; consonants := [bcdfghlmnpqrstvwxyz ']+
;; or
;; word := (wovels consonants)*
;; wovels :=  [aeiou]+
;; consonants := [bcdfghlmnpqrstvwxyz ']+

(defclass parsed-word (parsed-file)
  ((tokens
    :initarg :tokens
    :initform '()
    :accessor tokens)
   (the-method
    :initarg :the-method
    :initform :wovels-consonants
    :accessor the-method)))

(define-tokenizer (parsed-word +vowels+ +consonants+))

(defun vowel-p (letter)
  (cl-ppcre:scan +vowels+ (string letter)))

(defun consonant-p (letter)
  (cl-ppcre:scan +consonants+ (string letter)))

(define-parser-skeleton random-names name parsed-word)

(defmacro gen-parse-letters (name test)
  (alexandria:with-gensyms (token)
    (let ((function-name (alexandria:format-symbol t "~:@(parse-~a~)" name)))
      `(defun ,function-name ()
         (if (peek-valid-stream)
             (let-noerr ((,token (peek-token *file*)))
               (if (,test ,token)
                   (append
                    (list (next-token *file*))
                    (,function-name))
                   nil))
             nil)))))

(gen-parse-letters vowels vowel-p)

(gen-parse-letters consonants consonant-p)

(defun parse-w-c-w ()
  (when (peek-valid-stream)
    (let-noerr ((vowels-1 (parse-vowels))
                (consonants (parse-consonants))
                (vowels-2 (parse-vowels)))

      (push (append vowels-1 consonants vowels-2)
            (tokens *file*))
      (if (peek-valid-stream)
          (progn
            (decrement-pointer *file*)
            (parse-w-c-w))
          (progn
            (setf (alexandria:last-elt (tokens *file*))
                  (append (list +starter+)
                          (alexandria:last-elt (tokens *file*))))

            (setf (tokens *file*)
                  (reverse
                   (mapcar #'(lambda (a)
                               (reduce #'(lambda (c d) (concatenate 'string c d)) a))
                           (tokens *file*)))))))))

(defun parse-w-c (&optional (count 0))
  (when (peek-valid-stream)
    (let-noerr ((cluster-1 (if (= (mod count 2) 0)
                               (parse-vowels)
                               (parse-consonants)))
                (cluster-2 (if (= (mod count 2) 0)
                               (parse-consonants)
                               (parse-vowels))))
      (push (append cluster-1 cluster-2)
            (tokens *file*))
      (if (peek-valid-stream)
          (progn
            (decrement-pointer *file*)
            (parse-w-c (1+ count)))
          (progn
            (setf (alexandria:last-elt (tokens *file*))
                  (append (list +starter+)
                          (alexandria:last-elt (tokens *file*))))

            (setf (tokens *file*)
                  (reverse
                   (mapcar #'(lambda (a)
                               (reduce #'(lambda (c d) (concatenate 'string c d)) a))
                           (tokens *file*)))))))))

(defun parse-word ()
  (ecase (the-method *file*)
    (:wovels-consonants-wovels
      (parse-w-c-w))
    (:wovels-consonants
     (parse-w-c))))

(defparameter *db* '())

(defun tokenize (word)
  (with-name-file (:buffer word)
    (parse-word)
   (values (tokens *file*) *parsing-errors*)))

(defun find-tree-with-token (token)
  (find-if #'(lambda (tree) (traverse-find-if-tree tree token :test #'string=
                                                   :key #'(lambda (v) (elt v 0))))
           *db*))

(defun find-tree-with-first-token (token)
  (find-if #'(lambda (tree) (find-first-token tree token)) *db*))

(defun find-token (tree token)
  (traverse-find-if-tree tree token :test #'string=
                         :key #'(lambda (v) (elt v 0))))

(defun find-first-token (tree token)
  (string= (elt (elt tree 0) 0) token))

(defun insert-word (word)
  (multiple-value-bind (tokens errors)
      (tokenize word)
    (unless errors
      (loop for i from 0 below (length tokens) do
           (if (= 0 i)
               (when (not (find-tree-with-first-token (nth i tokens)))
                 (insert-value nil (nth i tokens)))
               (insert-value (nth (1- i) tokens) (nth i tokens)))))))

(defun insert-value (parent token)
  (if parent ;; not the first token in the list
      (let ((tree (find-tree-with-first-token parent)))
        (when tree ;;useless, parent always found
          (if (find-token tree token)
              (update-frequency tree token) ;; update freq
              (progn
                (traverse-nadd-child tree parent
                                     (initialize-node token)
                                     :test #'string=
                                     :key #'(lambda (v) (elt v 0)))
                (initialize-tree token)))));; add new child
      (initialize-tree token))) ;; create new tree

(defun initialize-node (token &optional (freq 1.0))
  (vector token freq))

(defun initialize-tree (token)
  (push (list (initialize-node token 1.0)) *db*))

(defun update-frequency (tree token)
  (traverse-napply-tree #'(lambda (node)
                            (if (string= (elt node 0) token)
                                (vector (elt node 0) (1+ (elt node 1)))
                                node))
                        tree))

(defun get-root-string (tree)
 (alexandria:first-elt (alexandria:first-elt tree)))

(defun generate-tokens (&optional (start nil))
  (let* ((all-starts (remove-if
                      #'(lambda (tree)
                          (not (string= (string (alexandria:first-elt
                                            (get-root-string tree)))
                                        +starter+)))
                      *db*))
         (actual-start (or start
                           (get-root-string
                            (nth (mod (num-utils:lcg-next) (length all-starts)) all-starts))))
         (chosen-tree (find-tree-with-first-token actual-start))
         (all-freqs (mapcar #'(lambda (n) (elt (alexandria:first-elt n) 1))
                            (rest chosen-tree)))
         (all-tokens (mapcar #'(lambda (n) (elt (alexandria:first-elt n) 0))
                             (rest chosen-tree)))
         (chosen (and all-tokens
                      (elt all-tokens
                           (num:random-select-by-frequency* all-freqs
                                                            :sort t
                                                            :normalize t)))))
    (if chosen
        (append (and (not start)
                     (list actual-start))
                (list chosen)
                (generate-tokens chosen))
        (if start
            nil
            (list actual-start)))))

(defun cut-ends (token)
  (subseq token 1 (length token)))

(defun generate ()
  (let ((chosen (mapcar #'cut-ends (generate-tokens))))
    (reduce #'(lambda (a b) (concatenate 'string a b)) chosen :initial-value "")))

(defun save-db (path)
  (let ((actual-path (res:get-resource-file path +names-resource+
                                            :if-does-not-exists :return-writable)))
    (with-open-file (stream actual-path :direction :output :if-exists :supersede
                            :if-does-not-exist :create)
      (format stream "~s" *db*))))

(defun load-db (path)
  (let ((actual-path (res:get-resource-file path +names-resource+)))
    (with-open-file (stream actual-path :direction :input :if-does-not-exist :error)
      (setf *db* (read stream)))))

(defun load-db* (resource path)
  (let ((actual-path (res:get-resource-file path resource)))
    (with-open-file (stream actual-path :direction :input :if-does-not-exist :error)
      (setf *db* (read stream)))))

(defun populate-db (path)
  (with-open-file (stream path :direction :input :if-does-not-exist :error)
    (let ((names (do ((line (read-line stream nil nil) (read-line stream nil nil))
                      (res '()))
                     ((null line) res)
                   (push line res))))
      (dolist (name names)
        (misc:dbg "~a" name)
        (when name
          (insert-word name))))))
