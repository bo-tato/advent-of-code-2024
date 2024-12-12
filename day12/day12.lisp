(in-package :advent-of-code-2024)

(defun perimeter (points map)
  (loop for point in points
        sum (loop for neighbor in (neighbors point)
                  count (not (eql (@ map point)
                                  (@ map neighbor))))))

(defun sides (points map)
  (loop with seen = (dict)
        for point in points
        for char = (@ map point)
        sum (loop for direction in '(:up :down :left :right)
                  for neighbor = (move point direction)
                  when (flet ((line-check-helper (increment)
                                  (loop initially (setf border point)
                                        for border = (move border (right-turn direction) increment)
                                        while (and (eql char (@ map border))
                                                   (not (eql char (@ map (move border direction)))))
                                        never (@ seen (cons border direction)))))
                         (and (not (eql char (@ map neighbor)))
                              (line-check-helper 1)
                              (line-check-helper -1)))
                    do (setf (@ seen (cons point direction)) t)
                    and sum 1)))

(let ((map (parse-map (read-file-into-string "input.txt")))
      (visited (dict)))
  (loop for point being the hash-keys of map
        for region = (flood-fill point map visited)
        when region
          sum (* (length region)
                 (sides region map))))
