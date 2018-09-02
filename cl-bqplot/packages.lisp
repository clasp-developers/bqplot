(defpackage "BQPLOT"
  (:shadow #:close #:map #:max #:min #:clear)
  (:use :common-lisp)
  (:import-from :fredokun-utilities #:[] #:[]-contains #:logg)
  (:export
   #:Keep
   ;; scale types
   #:mercator
   #:albers
   #:albers-usa
   #:equi-rectangular
   #:orthographic
   #:gnomonic
   #:stereographic
   #:linear-scale
   #:log-scale
   #:date-scale
   #:ordinal-scale
   #:color-scale
   #:date-color-scale
   #:ordinal-color-scale
   )
  (:documentation "Implements the bqplot package"))

