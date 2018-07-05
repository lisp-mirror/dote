;; dawn of the Era: a tactical game.
;; Copyright (C) 2018  cage

;; This  program is  free  software: you  can  redistribute it  and/or
;; modify it  under the  terms of  the GNU  General Public  License as
;; published by the Free Software  Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT  ANY  WARRANTY;  without   even  the  implied  warranty  of
;; MERCHANTABILITY or FITNESS  FOR A PARTICULAR PURPOSE.   See the GNU
;; General Public License for more details.

;; You should have  received a copy of the GNU  General Public License
;; along      with      this      program.       If      not,      see
;; <http://www.gnu.org/licenses/>.

(in-package :md2-mesh)

(define-constant +w-memory-target-id+             :target-id              :test #'eq)

(define-constant +w-memory-path-struct+           :path-struct            :test #'eq)

(define-constant +w-memory-action-did-costs+      :action-did-cost        :test #'eq)

(define-constant +w-memory-spell+                 :spell                  :test #'eq)

(define-constant +w-memory-interrupting-by-id+    :interrupting-by-id     :test #'eq)

(define-constant +w-memory-saved-interrupting-id+ :saved-interrupting-id  :test #'eq)

(define-constant +channel-planner-timeout+        0.001                   :test #'=)
