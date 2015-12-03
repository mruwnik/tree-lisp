;;;; tree.asd

(asdf:defsystem #:tree-sim
  :serial t
  :description "simulates the growth of a tree"
  :author "Daniel O'Connell"
  :license "GPL"
  :depends-on (#:split-sequence
	       #:cl-opengl
	       #:cl-glu
	       #:cl-glut)
  :components ((:file "package")
	       (:file "settings")
	       (:file "glm")
	       (:file "classes")
	       (:file "sun")
	       (:file "health")
	       (:file "diffuse")
	       (:file "growth")
	       (:file "tree")
	       (:file "tree-sim")
	       (:file "shading")
               (:file "display")))

