(in-package :advent-of-code-2024)

;; part1
(loop for line in (read-file-lines "input.txt")
      for (x y vx vy) = (string-to-num-list line)
      for final-x = (mod (+ x (* 100 vx)) 101)
      for final-y = (mod (+ y (* 100 vy)) 103)
      when (and (<= 0 final-x 49)
                (<= 0 final-y 50))
        sum 1 into quadrant1
      when (and (<= 51 final-x 100)
                (<= 0 final-y 50))
        sum 1 into quadrant2
      when (and (<= 0 final-x 49)
                (<= 52 final-y 102))
        sum 1 into quadrant3
      when (and (<= 51 final-x 100)
                (<= 52 final-y 102))
        sum 1 into quadrant4
      finally (return (* quadrant1 quadrant2 quadrant3 quadrant4)))

;; part2
(defun entropy (numbers)
  (- (loop for f in (hash-table-values (frequencies numbers))
           for p = (/ f 102)
           sum (* p (log p)))))

(loop for robots = (string-to-num-lists (read-file-into-string "input.txt"))
      for turn from 0
      for points = (loop for (x y vx vy) in robots
                         collect (cons (mod (+ x (* turn vx)) 101)
                                       (mod (+ y (* turn vy)) 103)))
      for x-entropy = (entropy (mapcar #'car points))
      for y-entropy = (entropy (mapcar #'cdr points))
      when (and (< x-entropy 13)
                (< y-entropy 13))
        do (format t "Turn: ~a, entropy: ~a,~a~%" turn x-entropy y-entropy)
           (loop for row below 103
                 do (loop for col below 101
                          do (write-char (if (find (cons row col) points :test 'equal)
                                             #\X
                                             #\.)))
                    (terpri))
           (return))
