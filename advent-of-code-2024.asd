;;;; advent-of-code-2024.asd

(asdf:defsystem #:advent-of-code-2024
  :depends-on (:serapeum :alexandria :cl-ppcre :str :array-operations :trivia
               :cl-interpol :metabang-bind :fn :series :function-cache :graph
               :place-utils :fset :screamer)
  :components ((:file "package")
               (:file "advent-of-code-2024")))
