(in-package :advent-of-code-2024)

(defun || (x y)
  (+ (* x (expt 10 (ceiling (log (1+ y) 10))))
     y))

(screamer::defun screamer-reduce (f seq &optional acc)
  (cond
    ((null acc) (screamer-reduce f (rest seq) (first seq)))
    ((consp seq) (screamer-reduce f (rest seq)
                                  (screamer:funcall-nondeterministic f acc (first seq))))
    (t acc)))

(screamer::defun operator (x y)
  (screamer:either
    (+  x y)
    (*  x y)
    (|| x y)))

(loop for (result . test-values) in (string-to-num-lists (read-file-into-string "input.txt"))
      when (screamer:possibly? (= result (screamer-reduce #'operator test-values)))
        sum result)
