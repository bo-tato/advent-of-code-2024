(in-package :advent-of-code-2024)
(in-readtable :aoc-sugar)

(let* ((graph (make-graph :directed? t))
       (map (parse-map (read-file-into-string "input.txt")))
       (flipped (flip-hash-table map))
       (start (@ flipped #\S))
       (end (@ flipped #\E)))
  (do-hash-table (point _ map)
    (dolist (direction '(:down :up :right :left))
      (add-node graph (cons point direction))))

  (do-hash-table (point char map)
    (when (find char ".S")
      (loop for direction in '(:down :up :right :left)
            for this-node = (cons point direction)
            for right-node = (cons point (right-turn direction))
            for forward-node = (cons (move point direction) direction)
            do (add-edge graph this-node right-node :weight 1000)
               (add-edge graph right-node this-node :weight 1000)
               (add-edge graph this-node forward-node))))

  (add-node graph "final")
  (dolist (direction '(:down :up :right :left))
    (add-edge graph (cons end direction) "final"))

  (multiple-value-bind (nodes distance)
      (find-shortest-path graph
                          (cons start :right)
                          "final"
                          :use-weights-p t)
    (format t "Part1: ~a~%" (1- distance))
    ;; part2
    ;; uses hacked version of find-shortest-path:
    ;; https://github.com/bo-tato/graph-utils/tree/all-nodes-on-shortest-path
    ;; that returns all nodes in any shortest path
    (loop for point being the hash-keys of map
          count (loop for direction in '(:down :up :right :left)
                        thereis (find (lookup-node graph (cons point direction)) nodes)))))
