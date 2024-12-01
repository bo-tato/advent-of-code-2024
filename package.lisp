;;;; package.lisp

(defpackage :advent-of-code-2024
  (:use :cl :alexandria :serapeum)
  (:import-from :uiop
   :read-file-lines)
  (:import-from :ppcre
   :register-groups-bind :scan-to-strings :all-matches-as-strings)
  (:import-from :array-operations/utilities
   :multf)
  (:import-from :metabang-bind
   :lambda-bind :bind)
  (:import-from :named-readtables
   :in-readtable)
  (:import-from :series
   :collect-length :until-if :series :collecting-fn)
  (:import-from :trivia
   :if-match :match)
  (:import-from :function-cache
   :clear-cache :defcached)
  (:import-from :graph
   :digraph)
  (:import-from :sb-unicode
   :digit-value)
  (:import-from :place-utils
   :oldf :with-resolved-places)
  (:import-from :fset
   :contains? :with :empty-set))
