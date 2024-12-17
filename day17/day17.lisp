(in-package :advent-of-code-2024)

(destructuring-bind (a b c &rest program)
    (string-to-num-list (read-file-into-string "input.txt"))
  (flet ((combo (operand)
           (ecase operand
             ((0 1 2 3) operand)
             (4 a)
             (5 b)
             (6 c))))
    ;; part2 done with hacky manual brute force, fixing the rightmost digits of a
    ;; when it makes a sequence that matches prefix of program
    (loop for value = #o00000510045136764 then (+ value #o1000000000000)
          do (setf a value)
          when (equal '(2 4 1 2 7 5 4 5 1 3 5 5 0 3 3 0)
                      (loop with output
                            for instruction-pointer = 0 then (+ 2 instruction-pointer)
                            for (opcode operand) = (nthcdr instruction-pointer program)
                            while opcode
                            do (ecase opcode
                                 (0 (setf a (truncate (/ a (expt 2 (combo operand))))))
                                 (1 (setf b (logxor b operand)))
                                 (2 (setf b (mod (combo operand) 8)))
                                 (3 (unless (zerop a)
                                      (setf instruction-pointer (- operand 2))))
                                 (4 (setf b (logxor b c)))
                                 (5 (push (mod (combo operand) 8) output))
                                 (6 (setf b (truncate (/ a (expt 2 (combo operand))))))
                                 (7 (setf c (truncate (/ a (expt 2 (combo operand)))))))
                            finally (return (nreverse output))))
            return value)))
