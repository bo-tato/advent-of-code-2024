(in-package :advent-of-code-2024)
(in-readtable :aoc-sugar)

(defun lockp (schematic)
  (every (eqls #\#) (first (lines schematic))))

(defun fitsp (lock key)
  (every (lambda (h1 h2)
           (< (+ h1 h2) 6))
         lock key))

(loop for schematic in (str:split #?"\n\n" (read-file-into-string "input.txt"))
      for heights = (mapcar λ(1- (count #\# _))
                            (transpose (lines schematic)))
      if (lockp schematic)
        collect heights into locks
      else
        collect heights into keys
      finally (return
                (loop for lock in locks
                      sum (loop for key in keys
                                count (fitsp lock key)))))
