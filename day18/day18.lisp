(in-package :advent-of-code-2024)
(in-readtable :aoc-sugar)

(defun find-path-with-lines (num-lines)
  (let ((graph (make-graph)))
    (loop for row upto 70
          do (loop for col upto 70
                   do (add-node graph (cons row col))))
    (map-nodes (lambda (node _id)
                 (mapcar λ(when (lookup-node graph _)
                            (add-edge graph node _))
                         (neighbors node)))
               graph)
    (loop for (row col) in (string-to-num-lists (read-file-into-string "input.txt"))
      repeat num-lines
      do (delete-node graph (lookup-node graph (cons row col))))
    (find-shortest-path graph '(0 . 0) '(70 . 70))))

;; part1
(find-path-with-lines 1024)

(loop with min = 0
      and max = 3451
      for mid = (truncate (/ (+ min max) 2))
      while (> (- max min) 2)
      do (if (find-path-with-lines mid)
             (setf min mid)
             (setf max mid))
         (format t "min: ~a, max: ~a~%" min max))
