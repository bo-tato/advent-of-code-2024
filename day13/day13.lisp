(in-package :advent-of-code-2024)
(in-readtable :aoc-sugar)
(use-package :screamer)

(defun tokens-for-machine (machine part2)
  (destructuring-bind (a-x a-y b-x b-y prize-x prize-y)
      (string-to-num-list machine)
    (when part2
        (incf prize-x 10000000000000)
        (incf prize-y 10000000000000))
    (one-value
        (let ((a-pushes (an-integer-abovev 0))
              (b-pushes (an-integer-abovev 0)))
          (assert! (=v prize-x (+v (*v a-pushes a-x)
                                   (*v b-pushes b-x))))
          (assert! (=v prize-y (+v (*v a-pushes a-y)
                                   (*v b-pushes b-y))))
          (assert! (andv (<=v (*v a-pushes a-x) prize-x)
                         (<=v (*v a-pushes a-y) prize-y)
                         (<=v (*v b-pushes b-x) prize-x)
                         (<=v (*v b-pushes b-y) prize-y)))
          (+v (*v 3 a-pushes) b-pushes))
        0)))

(loop for machine in (str:split #?"\n\n" (read-file-into-string "input.txt"))
      sum (tokens-for-machine machine nil) into part1
      sum (tokens-for-machine machine t) into part2
      finally (format t "Part1: ~a, Part2: ~a~%" part1 part2))
