(in-package :advent-of-code-2024)

(loop for line in (read-file-lines "input.txt")
      for (x y) = (string-to-num-list line)
      collect x into left-list
      collect y into right-list
      finally
         (format t "Part2: ~a~%"
                 (loop for x in left-list
                       sum (* x (count x right-list))))
         (format t "Part1: ~a~%"
                 (loop for x in (sort left-list #'<)
                       for y in (sort right-list #'<)
                       sum (abs (- x y)))))
