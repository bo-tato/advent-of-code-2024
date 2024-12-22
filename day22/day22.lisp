(in-package :advent-of-code-2024)

(defun prune (secret)
  (mod secret 16777216))

(defun mix (value secret)
  (logxor secret value))

(defun next-secret (secret)
  (let* ((secret (prune (mix (* secret 64) secret)))
         (secret (prune (mix (floor secret 32) secret))))
    (prune (mix (* secret 2048) secret))))

(loop for secret in (string-to-num-list (read-file-into-string "test.txt"))
      sum (repeat-until-stable #'next-secret secret :max-depth 2000))
