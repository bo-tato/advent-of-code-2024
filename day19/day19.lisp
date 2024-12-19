(in-package :advent-of-code-2024)

(defcached arrange-towels (patterns design)
  (if (str:emptyp design)
      1
      (loop for pattern in patterns
            if (str:starts-with-p pattern design)
              sum (arrange-towels patterns (subseq design (length pattern))))))

(loop with (p1 p2) = (str:split #?"\n\n" (read-file-into-string "input.txt"))
      with patterns = (words p1)
      and designs = (lines p2)
      for design in designs
      count (ppcre:scan `(:sequence :start-anchor
                         (:greedy-repetition 1 nil (:alternation ,@patterns))
                         :end-anchor)
                       design)
        into part1
      sum (arrange-towels patterns design) into part2
      finally (format t "Part1: ~a, Part2: ~a~%" part1 part2))
