(in-package :advent-of-code-2024)

(summing
  (ppcre:do-register-groups (x y) (#?/mul\((\d+),(\d+)\)/
                                   (ppcre:regex-replace-all (ppcre:create-scanner #?/don't\(\).*?do\(\)/ :single-line-mode t)
                                                            (read-file-into-string "input.txt")
                                                            ""))

    (sum (* (parse-integer x) (parse-integer y)))))
