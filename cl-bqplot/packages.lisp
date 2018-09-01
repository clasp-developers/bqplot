(defpackage "BQPLOT"
  (:shadow #:close #:map #:max #:min #:clear)
  (:use :common-lisp
        :cl-jupyter
        :cl-ipywidgets)
  (:import-from :fredokun-utilities #:[] #:[]-contains)
  (:export
   #:Keep
   )
  (:documentation "Implements the bqplot package"))

