(in-package :advent-of-code-2024)

(defun list-reader-macro (stream char)
  (declare (ignore char))
  `(list ,@(read-delimited-list #\] stream t)))

(defun dict-reader-macro (stream char)
  (declare (ignore char))
  `(serapeum:dict ,@(read-delimited-list #\} stream t)))

(named-readtables:defreadtable
    :aoc-sugar
  (:merge
   ;; λ(* 2 _) style lambda shorthand syntax
   :fn.reader
   ;; string interpolation and regex literals
   ;; see: http://edicl.github.io/cl-interpol/
   :interpol-syntax)
  (:macro-char #\[ #'list-reader-macro)
  (:macro-char #\] (get-macro-character #\)))
  (:macro-char #\{ #'dict-reader-macro)
  (:macro-char #\} (get-macro-character #\))))

(defun string-to-num-list (string)
  "Return a list of all numbers in STRING."
  (mapcar #'parse-integer (ppcre:all-matches-as-strings "[-\\d]+" string)))

(defun string-to-num-lists (string)
  "Return a list containing a list of all numbers for each line of STRING."
  (mapcar #'string-to-num-list (lines string)))

(defun point+ (point1 point2)
  (cons (+ (car point1) (car point2))
        (+ (cdr point1) (cdr point2))))

(defun point- (point1 point2)
  (cons (- (car point1) (car point2))
        (- (cdr point1) (cdr point2))))

(defun point-in-bounds-p (point rows cols)
  (and (<= 0 (car point) (1- rows))
       (<= 0 (cdr point) (1- cols))))

(defun row+ (point &optional (distance 1))
  "Add DISTANCE to the row of POINT."
  (destructuring-bind (row . col) point
    (cons (+ row distance) col)))

(defun row- (point &optional (distance 1))
  "Subtract DISTANCE from the row of POINT."
  (destructuring-bind (row . col) point
    (cons (- row distance) col)))

(defun col+ (point &optional (distance 1))
  "Add DISTANCE to the col of POINT."
  (destructuring-bind (row . col) point
    (cons row (+ col distance))))

(defun col- (point &optional (distance 1))
  "Subtract DISTANCE from the col of POINT."
  (destructuring-bind (row . col) point
    (cons row (- col distance))))

(defun taxicab-distance (point1 point2)
  "Returns the taxicab distance between POINT1 and POINT2."
  (destructuring-bind (row-diff . col-diff)
      (point- point1 point2)
    (+ (abs row-diff)
       (abs col-diff))))

(defun neighbors (point &key directions)
  "Returns a list of the four points adjacent to POINT."
  (loop for f in '(row+ row- col+ col-)
        for direction in '(:down :up :right :left)
        for neighbor = (funcall f point)
        if directions
          collect (cons neighbor direction)
        else
          collect neighbor))

(defun parse-direction (char)
  "Returns direction for the chars ^v<>"
  (ecase char
    (#\^ :up)
    (#\v :down)
    (#\< :left)
    (#\> :right)))

(defun move (point direction &optional (distance 1))
  "Return the the coordinates DISTANCE away from POINT in DIRECTION."
  (ecase direction
    (:up (row- point distance))
    (:down (row+ point distance))
    (:left (col- point distance))
    (:right (col+ point distance))))

(defun right-turn (direction)
  "Rotates DIRECTION to the right 90 degrees."
  (ecase direction
    (:up :right)
    (:right :down)
    (:down :left)
    (:left :up)))

(defun cross-product (point1 point2)
  (bind (((x1 . y1) point1)
         ((x2 . y2) point2))
    (- (* x1 y2) (* x2 y1))))

;; https://en.wikipedia.org/wiki/Shoelace_formula
(defun shoelace (points)
  "Compute the area of the polygon defined by POINTS."
  (abs (/ (loop for prev = (lastcar points) then point
                for point in points
                sum (cross-product prev point))
          2)))

(defun remove-nth (n seq)
  "Remove element with index N from SEQ."
  (append (subseq seq 0 n)
          (subseq seq (1+ n))))

(defun transpose (lists)
  "Transpose a list of LISTS."
  (apply #'mapcar #'list lists))

(defun count-subseq (sequence-1 sequence-2)
  "Returns the number of matches for SEQUENCE-1 within SEQUENCE-2."
  (loop for pos = (search sequence-1 sequence-2)
          then (search sequence-1 sequence-2 :start2 (1+ pos))
        while pos
        sum 1))

(defun parse-map (string &key (parse #'identity))
  "Returns a hash table mapping (row . col) to the character at that position in
  STRING."
  (loop with map = (dict)
        for line in (lines string)
        for row from 0
        do (loop for char across line
                 for col from 0
                 do (setf (@ map (cons row col)) (funcall parse char)))
        finally (return map)))

(defun print-map (map)
  "Prints MAP, a hash-map of (row . col) to characters."
  (loop with rows = (apply #'max (mapcar #'car (hash-table-keys map)))
        with cols = (apply #'max (mapcar #'cdr (hash-table-keys map)))
        for row upto rows
        do (loop for col upto cols
                 do (write-char (@ map (cons row col))))
           (terpri)))

(defun digits (number)
  "Returns a list of digits in NUMBER."
  (loop with result
        for n = number then (floor n 10)
        while (plusp n)
        do (push (mod n 10) result)
        finally (return result)))

(defun digits-to-num (digits)
  "Converts a list of DIGITS to a number."
  (loop for n = 0 then (+ (* 10 n) digit)
        for digit in digits
        finally (return n)))

(defun flood-fill (start map visited)
  "Returns a list of points forming the contiguous region around START that all
  contain the same char in MAP. VISITED is updated with the points in the
  region."
  (loop with points = (list start)
        and char = (@ map start)
        for point = (pop points) while point
        when (not (@ visited point))
          do (setf (@ visited point) t)
             (loop for neighbor in (neighbors point)
                   when (eql char (@ map neighbor))
                     do (push neighbor points))
          and collect point))
