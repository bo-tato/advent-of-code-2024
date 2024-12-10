(in-package :advent-of-code-2024)
(in-readtable :aoc-sugar)

(defparameter *map* (parse-map (read-file-into-string "input.txt")
                               :parse #'digit-char-p))

(defun trailp (start end)
  (if (equal start end)
      1
      (loop for neighbor in (neighbors start)
            when (eql (@ *map* neighbor) (1+ (@ *map* start)))
              sum (trailp neighbor end))))

(loop with trail-ends = (loop for end being the hash-keys of *map* using (hash-value height)
                              when (= height 9)
                                collect end)
      for start being the hash-keys of *map* using (hash-value height)
      when (zerop height)
        sum (count-if λ(plusp (trailp start _)) trail-ends) into part1
        and sum (sum (mapcar λ(trailp start _) trail-ends)) into part2
      finally
         (format t "Part1: ~a~%Part2: ~a~%" part1 part2))
