(in-package :advent-of-code-2024)
(in-readtable :aoc-sugar)

(defun antinodes (antenna1 antenna2)
  (let ((distance (point- antenna2 antenna1)))
    (list (point- antenna1 distance)
          (point+ antenna2 distance))))

(defun antinodes-part2 (antenna1 antenna2 rows cols)
  (let ((distance (point- antenna2 antenna1)))
    (loop for point1 = antenna2 then (point+ point1 distance)
          and point2 = antenna1 then (point- point2 distance)
          while (or (point-in-bounds-p point1 rows cols)
                    (point-in-bounds-p point2 rows cols))
          collect point1
          collect point2)))

(loop with antennas = (dict)
      and antinodes
      and input = (lines (read-file-into-string "input.txt"))
      with rows = (length input)
      and cols = (length (first input))
      for row from 0
      for line in input
      do (loop for col from 0
               for char across line
               unless (char= char #\.)
                 do (push (cons row col) (@ antennas char)))
      finally (loop for positions being the hash-values of antennas
                    do (map-combinations
                        (lambda-match1 (list antenna1 antenna2)
                          (appendf antinodes (antinodes-part2 antenna1 antenna2
                                                              rows cols)))
                        positions
                        :length 2))
              (return (length (remove-duplicates
                               (filter λ(point-in-bounds-p _ rows cols) antinodes)
                               :test 'equal))))
