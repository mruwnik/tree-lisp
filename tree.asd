;;;; tree.asd

(asdf:defsystem #:tree
  :serial t
  :description "simulates the growth of a tree"
  :author "Daniel O'Connell"
  :license "GPL"
  :depends-on (#:cl-opengl
	       #:cl-glu
	       #:cl-glut)
  :components ((:file "package")
	       (:file "file-utils")
	       (:file "classes")
	       (:file "sun")
	       (:file "tree")
               (:file "display")))

