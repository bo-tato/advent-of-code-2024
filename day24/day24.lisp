(in-package :advent-of-code-2024)
(in-readtable :aoc-sugar)

(defparameter *wires* (dict))

(ppcre:do-register-groups (wire value)
    ("(.*): ([01])" (read-file-into-string "input.txt"))
  (setf (@ *wires* wire) (parse-integer value)))

(do (done)
    (done)
  (setf done t)
  (ppcre:do-register-groups (wire1 op wire2 output)
      (#?r"(\w+) (AND|OR|XOR) (\w+) -> (\w+)" (read-file-into-string "input.txt"))
    (if-let ((wire1 (@ *wires* wire1))
             (wire2 (@ *wires* wire2)))
      (setf (@ *wires* output)
            (case (intern op)
              (and (logand wire1 wire2))
              (or  (logior wire1 wire2))
              (xor (logxor wire1 wire2))))
      (nix done))))

(parse-integer
 (fmt "~{~a~}"
      (mapcar #'cdr
              (sort (filter λ(str:starts-with-p "z" _)
                            (hash-table-alist *wires*)
                            :key #'car)
                    #'string> :key #'car)))
 :radix 2)
