;;;; tree.asd

(asdf:defsystem #:tree-sim
  :serial t
  :description "simulates the growth of a tree"
  :author "Daniel O'Connell"
  :license "GPL"
  :depends-on (
         #:clinch
         #:split-sequence)
  :components ((:file "package")
	       (:file "settings")
	       (:file "glm")
	       (:file "classes")
	       (:file "sun")
	       (:file "health")
	       (:file "diffuse")
	       (:file "growth")
	       (:file "tree-sim")
         (:file "models")
         (:file "draw-parts")
         (:file "display")
  ))
