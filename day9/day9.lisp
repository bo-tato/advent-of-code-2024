(in-package :advent-of-code-2024)

(defun calculate-checksum (file-id position length)
  (loop repeat length
        for pos from position
        sum (* pos file-id)))

(defun solve (files free-space consuming-file-p position checksum)
  (declare (optimize speed))            ; needed for SBCL to optimize tail recursion
  (if (null files)
      checksum
      (if consuming-file-p
          (let-match1 (cons id length) (dlist-pop files)
            (solve files free-space nil (+ position length)
                   (+ checksum (calculate-checksum id position length))))
          (let-match1 (cons file-id file-length) (dlist-pop files :from-end t)
            (if-let (free-length (pop free-space))
              (if (< free-length file-length)
                  (solve (dlist-push (cons file-id (- file-length free-length)) files :at-end t)
                         free-space
                         t
                         (+ position free-length)
                         (+ checksum (calculate-checksum file-id position free-length)))
                  (solve files
                         (push (- free-length file-length) free-space)
                         nil
                         (+ position file-length)
                         (+ checksum (calculate-checksum file-id position file-length))))
              (solve files nil t position checksum))))))

(let* ((input (mapcar #'digit-char-p
                      (coerce (str:trim (read-file-into-string "input.txt"))
                              'list)))
       (files (loop for length in input by #'cddr
                    for id from 0
                    collect (cons id length)))
       (free-space (loop for free in (rest input) by #'cddr collect free))
       file-positions free-positions)

  ;; part1
  ;; (solve (coerce files 'dlist) free-space t 0 0)

  ;; part2
  (loop for (file-id . file-length) in files
        for position = 0 then (+ position free-length)
        for free-length in free-space
        do (push position file-positions)
           (incf position file-length)
           (push position free-positions)
        finally
           (push position file-positions))

  (setf free-space (mapcar #'cons free-space (reverse free-positions)))
  (loop for (file-id . file-length) in (reverse files)
        for file-position in file-positions
        sum (loop for free-info in free-space
                  for (free-length . free-position) = free-info
                  while (< (cdr free-info) file-position)
                  when (<= file-length free-length)
                    do (decf (car free-info) file-length)
                       (incf (cdr free-info) file-length)
                       (return (calculate-checksum file-id free-position file-length))
                  finally
                     (return (calculate-checksum file-id file-position file-length)))))
