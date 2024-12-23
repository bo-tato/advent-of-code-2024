(in-package :advent-of-code-2024)
(in-readtable :aoc-sugar)

(defparameter *graph* (make-graph))

(loop for line in (read-file-lines "input.txt")
      for (node1 node2) = (words line)
      do (add-node *graph* node1)
         (add-node *graph* node2)
         (add-edge *graph* node1 node2))

(summing
  (map-nodes (lambda (node id)
               (when (str:starts-with? "t" node)
                 (map-combinations (lambda-match1 (list n1 n2)
                                     (and (edge-exists? *graph* n1 n2)
                                          (or (not (str:starts-with? "t" n1))
                                              (string< node n1))
                                          (or (not (str:starts-with? "t" n2))
                                              (string< node n2))
                                          (sum 1)))
                                   (graph-utils:neighbors *graph* node :return-ids? nil)
                                   :length 2)))
             *graph*))
