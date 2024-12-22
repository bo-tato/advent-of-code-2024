(in-package :advent-of-code-2024)

(defparameter *numeric-keypad* (parse-map #?"\
789
456
123
X0A"))
(remhash '(3 . 0) *numeric-keypad*)

(defparameter *directional-keypad* (parse-map #?"\
X^A
<v>"))
(remhash '(0 . 0) *directional-keypad*)

(defun shortest-path (pos1 pos2 target-keypad)
  (with-output-to-string (stream)
    (destructuring-bind (row-diff . col-diff)
        (point- pos2 pos1)
      (when (and (@ target-keypad (col+ pos1 col-diff))
                 (minusp col-diff))
        (dotimes (_ (abs col-diff))
          (write-char #\< stream)))
      (when (@ target-keypad (row+ pos1 row-diff))
        (dotimes (_ (abs row-diff))
          (if (minusp row-diff)
              (write-char #\^ stream)
              (write-char #\v stream))))
      (when (plusp col-diff)
        (dotimes (_ col-diff)
          (write-char #\> stream)))
      (when (not (@ target-keypad (row+ pos1 row-diff)))
        (dotimes (_ (abs row-diff))
          (if (minusp row-diff)
              (write-char #\^ stream)
              (write-char #\v stream))))
      (when (and (not (@ target-keypad (col+ pos1 col-diff)))
                 (minusp col-diff))
        (dotimes (_ (abs col-diff))
          (write-char #\< stream))))))

(defun find-shortest-input (outputs target-keypad)
  (with-output-to-string (stream)
      (loop with flipped-keypad = (flip-hash-table target-keypad)
            for position = (@ flipped-keypad #\A) then dest
            for output across outputs
            for dest = (@ flipped-keypad output)
            do (write-string (shortest-path position dest target-keypad) stream)
               (write-char #\A stream))))

(loop for code in (read-file-lines "input.txt")
      sum (* (~> code
                 (find-shortest-input *numeric-keypad*)
                 (find-shortest-input *directional-keypad*)
                 (find-shortest-input *directional-keypad*)
                 length)
             (parse-integer (str:trim-right code :char-bag "A"))))
