(in-package :advent-of-code-2024)
(in-readtable :aoc-sugar)

(defparameter *gates* (dict))
(ppcre:do-register-groups (wire1 op wire2 output)
    (#?r"(\w+) (AND|OR|XOR) (\w+) -> (\w+)" (read-file-into-string "input.txt"))
  (setf (@ *gates* (intern output)) (list (intern wire1) (intern op) (intern wire2))))

(defun simulate (wires)
  (declare (optimize speed))
  (do (done (progress t))
      ((or done (null progress))
       (when progress
         (fmt "~{~a~}"
              (mapcar #'cdr
                      (sort (filter λ(str:starts-with-p "z" _)
                                    (hash-table-alist wires)
                                    :key λ(string (car _)))
                            #'string> :key λ(string (car _)))))))
    (setf done t
          progress nil)
    (loop for (wire1 op wire2) being the hash-values of *gates*
            using (hash-key output)
          do (if-let ((wire1 (@ wires wire1))
                      (wire2 (@ wires wire2)))
               (unless (@ wires output)
                 (setf (@ wires output)
                       (case op
                         (and (logand wire1 wire2))
                         (or  (logior wire1 wire2))
                         (xor (logxor wire1 wire2)))
                       progress t))
               (nix done)))))

;; part1
(let ((wires (dict)))
  (ppcre:do-register-groups (wire value)
      ("(.*): ([01])" (read-file-into-string "input.txt"))
    (setf (@ wires (intern wire)) (parse-integer value)))
  (parse-integer (simulate wires) :radix 2))

(defun correct-digits ()
  (declare (optimize speed))
  (loop repeat 10
        for wires = (dict)
        for x-value = (random (expt 2 45))
        for y-value = (random (expt 2 45))
        for z-value = (fmt "~b" (+ x-value y-value))
        do (loop for i upto 44
                 do (setf (@ wires (intern (fmt "x~2,'0d" i))) (ldb (byte 1 i) x-value))
                    (setf (@ wires (intern (fmt "y~2,'0d" i))) (ldb (byte 1 i) y-value)))
        minimize (length (str:suffix (list z-value (simulate wires))))))

;; for part2 I just ran this manually four times, it discovers a new pair each time
(defparameter *correct-digits* (correct-digits))
(map-combinations (lambda-match1 (list output1 output2)
                    (rotatef (@ *gates* output1)
                             (@ *gates* output2))
                    (let ((correct (correct-digits)))
                      (when (> correct *correct-digits*)
                        (format t "swap: ~a ~a for ~a~%" output1 output2 correct)))
                    (rotatef (@ *gates* output1)
                             (@ *gates* output2)))
                  (hash-table-keys *gates*)
                  :length 2)
