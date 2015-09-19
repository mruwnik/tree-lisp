;;;; tree.asd

(asdf:defsystem #:tree
  :serial t
  :description "simulates the growth of a tree"
  :author "Daniel O'Connell"
  :license "GPL"
  :depends-on (#:split-sequence
	       #:cl-opengl
	       #:cl-glu
	       #:cl-glut)
  :components ((:file "package")
	       (:file "glm")
	       (:file "classes")
	       (:file "sun")
	       (:file "tree")
               (:file "display")))

