(in-package :advent-of-code-2024)

(defun safep (report)
  (if (> (first report) (lastcar report))
      (safep (reverse report))
      (loop for (a b) on report while b
            always (<= 1 (- b a) 3))))

(defun part2-safep (report)
  (loop for level below (length report)
        thereis (safep (remove-nth level report))))

(loop for report in (read-file-lines "input.txt")
      count (part2-safep (string-to-num-list report)))
