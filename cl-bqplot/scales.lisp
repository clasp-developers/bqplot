(in-package :bqplot)

(defclass scale (cljw:widget)
  ((scale-types :accessor scale-types
                :initform nil ;;;TODO: Fill in class names
                )
   (precedence :accessor precedence
               :initform 1
               :type integer)
   (domain-class :accessor domain-class
                 :type float
                 :initform 0)
   (reverse :accessor reverse
            :type bool
            :initform :null
            :metadata (:sync t
                             :json-name "reverse"))
   (%ipython-display :accessor ipython-display
                     :initform nil))
  (:default-initargs
   :view-name (cljw:unicode "Scale")
    :model-name (cljw:unicode "ScaleModel")
    :view-module (cljw:unicode "bqplot")
    :model-module (cljw:unicode "bqplot")
    ;;TODO: Fix view-module-version and model-module-version
    )
  (:metaclass traitlets:traitlet-class))

(defclass geo-scale (scale)
  ()
  (:default-initargs
   :view-name (cljw:unicode "GeoScale")
    :model-name (cljw:unicode "GeoScaleModel"))
  (:metaclass traitlets:traitlet-class))
                                  
    
