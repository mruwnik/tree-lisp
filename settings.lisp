(in-package #:tree-sim)

;; Basic simulation options
(defparameter *growth* T)
(defparameter *growth-ratio* NIL)
(defparameter *health-checks* T)
(defparameter *sunshine* NIL)
(defparameter *seasons* T)
(defparameter *use-supplies* NIL)

;; Control what supplies are taken into account
(defparameter *diffuse-minerals* NIL)
(defparameter *diffuse-water* NIL)
(defparameter *diffuse-sugars* NIL)
(defparameter *diffuse-auxin* NIL)
(defparameter *diffuse-growth* NIL)
(defparameter *diffuse-abscisic-acid* NIL)

;; Display options
(defparameter *draw-position* NIL)
(defparameter *draw-wind* NIL)
(defparameter *draw-tree* t)
(defparameter *draw-leaf-occulence* NIL)

(defparameter *show-help* NIL)
(defparameter *show-debug-info* NIL)

