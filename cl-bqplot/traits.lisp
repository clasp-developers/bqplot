(in-package :bqplot)

(defun array-from-json (value obj)
  (when value
    (when ([] value "values")
      (let ((type ([] value "type")))
        (cond ((string= type "float")
               ([] value "values"))
              ((string= type "date")
               (error "Handle date"))
              (t (error "Handle type ~s" type)))))))

(defun array-to-json (arr obj)
  (if arr
      (cond
        ((typep (array-element-type arr) '(or t number))
         (list (cons "type" "float")
               (cons "values" (let ((farray (make-array (length arr)
                                                        :element-type 'single-float)))
                                (loop for idx below (length arr)
                                      do (setf (aref farray idx) (float (aref arr idx) 1s0)))
                                farray))))
        (t (error "Handle array of type ~a" (array-element-type arr))))
      (list (cons "type" nil)
            (cons "values" arr))))

(defparameter *array-serialization*
  (list :from-json 'array-from-json
        :to-json 'array-to-json))
