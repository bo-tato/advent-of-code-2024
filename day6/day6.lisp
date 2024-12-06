(in-package :advent-of-code-2024)

(defun guard-visits (map start-pos start-direction)
  (loop with pos = start-pos
        and direction = start-direction
        and visited = (dict)
        while (@ map pos)
        for forward-pos = (move pos direction)
        if (@ visited (cons pos direction))
          return :inf
        else
          do (setf (@ visited (cons pos direction)) t)
             (if (eql (@ map forward-pos) #\#)
                 (setf direction (right-turn direction))
                 (setf pos forward-pos))
        finally (return (length (remove-duplicates (mapcar #'first (hash-table-keys visited)) :test 'equal)))))

(loop with map = (dict) and start-pos and start-direction
      and visited = (dict)
      for line in (read-file-lines "input.txt")
      for row from 0
      do (loop for char across line
               for col from 0
               for pos = (cons row col)
               when (char= char #\^)
                 do (setf start-pos pos
                          start-direction :up)
               do (setf (@ map pos) char))
      finally
         (format t "Part1: ~a~%" (guard-visits map start-pos start-direction))
         (format t "Part2: ~a~%"
                 (summing
                   (do-hash-table (pos char map)
                     (when (char= char #\.)
                       (setf (@ map pos) #\#)
                       (when (eq :inf (guard-visits map start-pos start-direction))
                         (sum 1))
                       (setf (@ map pos) #\.))))))
