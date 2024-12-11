(in-package :advent-of-code-2024)

(defcached expands-to (stone blinks)
  (let ((digits (digits stone))
        (blinks (1- blinks)))
    (cond
      ((< blinks 0) 1)
      ((zerop stone) (expands-to 1 blinks))
      ((evenp (length digits)) (multiple-value-bind (first-half second-half)
                                   (halves digits)
                                 (+ (expands-to (digits-to-num first-half) blinks)
                                    (expands-to (digits-to-num second-half) blinks))))
      (t (expands-to (* stone 2024) blinks)))))

(loop for stone in (string-to-num-list (read-file-into-string "input.txt"))
      sum (expands-to stone 75))
