(in-package #:tree-sim)

(deftest test-sort-triangle ()
  (check 
    (equalp (scaled-max-abs 1 1 1) 1)
    (equalp (scaled-max-abs 0 1 1) 0)
    (equalp (scaled-max-abs 1 0.1 10) 1/10)
    (equalp (scaled-max-abs 1 1.2 10) 1/10)
    (equalp (scaled-max-abs 1 0.09 10) 0.09)
    (equalp (scaled-max-abs 2 1 0.1) 1)
    (equalp (scaled-max-abs 2 19 0.1) 19)
    (equalp (scaled-max-abs 2 20 0.1) 20)
    (equalp (scaled-max-abs 2 21 0.1) 20)
    (equalp (scaled-max-abs 0.1 1 1) 0.1)
    (equalp (scaled-max-abs 0.1 1 2) 0.05)
    (equalp (scaled-max-abs 0.1 1 0.5) 0.2)
    (equalp (scaled-max-abs 0.1 0.19 0.5) 0.19)))
