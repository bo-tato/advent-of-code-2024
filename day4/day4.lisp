(in-package :advent-of-code-2024)

(defun diagonals (matrix)
  "Return a list of lists containing all diagonals from
top left to bottom right in MATRIX."
  (loop
    with size = (length matrix)
    for diag below size
    collect (loop for col from diag below size
                  for row in matrix
                  collect (nth col row))
    unless (zerop diag)
      collect (loop for row in (nthrest diag matrix)
                    for col from 0
                    collect (nth col row))))

(defun count-xmas (matrix)
  (loop for line in matrix
        sum (count-subseq "XMAS" line)
        sum (count-subseq "SAMX" line)))

;; part1
(loop with input = (loop for line in (read-file-lines "input.txt")
                         collect (coerce line 'list))
      for matrix in (list input (diagonals input)
                          (transpose input) (diagonals (mapcar #'reverse input)))
      sum (count-xmas matrix))

;; part2
(defun x-mas-p (c1 c2)
  (or (and (char= c1 #\M) (char= c2 #\S))
      (and (char= c1 #\S) (char= c2 #\M))))

(loop with grid = (make-array '(140 140))
      for line in (read-file-lines "input.txt")
      for row from 0
      do (loop for char across line
               for col from 0
               do (setf (aref grid row col) char))
      finally
         (format t "Part2: ~a~%"
                 (loop for row from 1 below 139
                       sum (loop for col from 1 below 139
                                 count (and (char= (aref grid row col) #\A)
                                            (and (x-mas-p (aref grid (1- row) (1- col))
                                                          (aref grid (1+ row) (1+ col)))
                                                 (x-mas-p (aref grid (1- row) (1+ col))
                                                          (aref grid (1+ row) (1- col)))))))))
