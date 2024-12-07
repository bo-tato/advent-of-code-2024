(in-package :advent-of-code-2024)

(defun concat-numbers (x y)
  (+ (* x (expt 10 (ceiling (log (1+ y) 10))))
     y))

(defun can-get-result-p (result test-values acc)
  (if-match (cons val rest) test-values
    (or (can-get-result-p result rest (* acc val))
        (can-get-result-p result rest (+ acc val))
        (can-get-result-p result rest (concat-numbers acc val)))
    (= result acc)))

(loop for (result . test-values) in (string-to-num-lists (read-file-into-string "input.txt"))
      when (can-get-result-p result (rest test-values) (first test-values))
        sum result)
