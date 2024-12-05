(in-package :advent-of-code-2024)
(in-readtable :aoc-sugar)

(defun follows-rules-p (update rules)
  (loop for (x y) in rules
        never (when-let ((x-pos (position x update))
                         (y-pos (position y update)))
                (> x-pos y-pos))))

(defun order-update (update rules)
  (loop with update = (copy-list update)
        for (x y) in rules
        do (when-let ((x-pos (position x update))
                      (y-pos (position y update)))
             (when (> x-pos y-pos)
               (rotatef (nth x-pos update)
                        (nth y-pos update))))
        finally (return update)))

(defun middle-page (update)
  (nth (floor (length update) 2)
       update))

(destructuring-bind (rules updates)
    (~>> (read-file-into-string "input.txt")
         (str:split #?"\n\n")
         (mapcar #'string-to-num-lists))
  (format t "Part1: ~a~%"
          (loop for update in updates
                when (follows-rules-p update rules)
                  sum (middle-page update)))
  (format t "Part2: ~a~%"
          (loop for update in updates
                unless (follows-rules-p update rules)
                  sum (middle-page
                       (repeat-until-stable λ(order-update _ rules)
                                            update
                                            :test 'equal)))))

