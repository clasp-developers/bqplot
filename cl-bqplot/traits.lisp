(in-package :bqplot)

(defun array-from-json (value &key obj)
  (when value
    (when ([] value "values")
      (let ((type ([] value "type")))
        (cond ((string= type "float")
               ([] value "values"))
              ((string= type "date")
               (error "Handle date"))
              (t (error "Handle type ~s" type)))))))

(defun array-to-json (arr &key obj)
  (if arr
      (cond
        ((typep (array-element-type arr) 'number)
         (list (cons "type" "float")
               (cons "values" arr)))
        (t (error "Handle array of type ~a" (array-element-type arr))))
      (error "Handle none")))

(defparameter *array-serialization*
  (list :from-json 'array-from-json
        :to-json 'array-to-json))
