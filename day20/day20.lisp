(in-package :advent-of-code-2024)

(defun cheats-over-100 (max-cheat-distance map distance-from-end)
  (summing
    (do-hash-table (start-cheat char map)
      (when (char= char #\.)
        (loop for row-offset from (- max-cheat-distance) to max-cheat-distance
              do (loop for col-offset from (- max-cheat-distance) to max-cheat-distance
                       for end-cheat = (col+ (row+ start-cheat row-offset) col-offset)
                       for cheat-distance = (taxicab-distance start-cheat end-cheat)
                       when (and (eql (@ map end-cheat) #\.)
                                 (<= cheat-distance max-cheat-distance)
                                 (>= (- (@ distance-from-end start-cheat)
                                        (@ distance-from-end end-cheat)
                                        cheat-distance)
                                     100))
                         do (sum 1)))))))

(let* ((map (parse-map (read-file-into-string "input.txt")))
       (flipped-map (flip-hash-table map))
       (start (@ flipped-map #\S))
       (end (@ flipped-map #\E))
       (distance-from-end (dict)))
  (setf (@ map start) #\.
        (@ map end) #\.)
  (loop with queue = (queue (cons end 0))
        for (point . distance) = (deq queue) while point
        when (not (@ distance-from-end point))
          do (setf (@ distance-from-end point) distance)
             (dolist (neighbor (neighbors point))
               (when (eql #\. (@ map neighbor))
                 (enq (cons neighbor (1+ distance)) queue))))
  (format t "Part1: ~a, Part2: ~a~%"
          (cheats-over-100 2 map distance-from-end)
          (cheats-over-100 20 map distance-from-end)))
