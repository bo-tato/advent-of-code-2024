(in-package :advent-of-code-2024)

(defun prune (secret)
  (mod secret 16777216))

(defun mix (value secret)
  (logxor secret value))

(defun next-secret (secret)
  (let* ((secret (prune (mix (* secret 64) secret)))
         (secret (prune (mix (floor secret 32) secret))))
    (prune (mix (* secret 2048) secret))))

(defun price-changes (secret)
  (loop initially (setf n secret)
        for last-price = (mod n 10)
        for n = (next-secret n)
        for price = (mod n 10)
        for (p1 p2 p3 p4) = (list (- price last-price)
                                  p1
                                  p2
                                  p3)
        repeat 2000
        when p4
          collect (list price p1 p2 p3 p4)))

;; part1
(loop for secret in (string-to-num-list (read-file-into-string "input.txt"))
      sum (repeat-until-stable #'next-secret secret :max-depth 2000))

;; part2
(loop with total-for-change = (dict)
      for secret in (string-to-num-list (read-file-into-string "input.txt"))
      do (loop with best-price = (dict)
               for (price . last-changes) in (price-changes secret)
               do (ensure (@ best-price last-changes) price)
               finally (do-hash-table (last-changes price best-price)
                         (incf (ensure (@ total-for-change last-changes) 0)
                             price)))
      finally (return (apply #'max (hash-table-values total-for-change))))
