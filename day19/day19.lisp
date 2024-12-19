(in-package :advent-of-code-2024)

(loop with (p1 p2) = (str:split #?"\n\n" (read-file-into-string "input.txt"))
      with patterns = (str:split ", " p1)
      and designs = (lines p2)
      for design in designs
      count (ppcre:scan `(:sequence :start-anchor
                         (:greedy-repetition 1 nil (:alternation ,@patterns))
                         :end-anchor)
                       design))
