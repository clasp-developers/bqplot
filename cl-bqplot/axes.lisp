(in-package :bqplot)

(defclass base-axis (widget)
  ((axis-types :initarg :axis-types :accessor axis-types
           :type list
           :Initform (List (cons "bqplot.Axis" "Axis")
                               (cons "bqplot.ColorAxis" "ColorAxis"))))
   (:default-initargs
    :view-module (cljw:unicode "bqplot")
     :model-module (cljw:unicode "bqplot")
     :view-module-version *frontend-version*
     :model-module-version *frontend-version*)
   (:metaclass traitlets:traitlet-class))

(defclass axis (base-axis)
  ((icon :accessor icon
	 :type unicode
	 :initform (cljw:unicode "fa-arrows"))	 
   (orientation :initarg :orientation :accessor orientation
		:type unicode
		:initform (cljw:unicode "horizontal")
		:metadata (:sync t
				 :json-name "orientation"))   
   (side :initarg :side :accessor side
	 :type unicode
	 :initform (cljw:unicode "")
	 :metadata (:sync t
			  :json-name "side"))
   (label-axes :accessor label-axes
	  :type unicode
	  :initform (cljw:unicode "")
	  :metadata (:sync t
			   :json-name "label"))
   (grid-lines :accessor grid-lines
	       :type unicode
	       :initform (cljw:unicode "solid")
	       :metadata (:sync t
				:json-name "grid_lines"))
   (tick-format :accessor tick-format
		:type unicode
		:initform (cljw:unicode "")
		:metadata (:sync t
				 :jsone-name "tick_format"))
   (scale :initarg :scale :accessor scale
	  :initform (make-instance 'scale)
	  :metadata (:sync t
			   :json-name "scale"
                                        ;,@cljw:*widget-serialization*
                           ))
   (num-ticks :accessor num-ticks
	      :type integer
	      :initform nil
	      :metadata (:sync t
			       :json-name "num_ticks"))
   (tick-values :accessor tick-values
		:type list
		:initform nil
		:metadata (:sync t
				 :json-name "tick_values"
				 ;*array-serialization*
				 ))
   (offset :accessor offset
	   :type list
	   :initform ()
	   :metadata (:sync t
			    :json-name "offset"
			    ;*array-serialization*
			    ))
   (label-location :accessor label-location
		   :type unicode 
		   :initform (cljw:unicode "middle")
		   :metadata (:sync t
				    :json-name "label_location"))
   (label-color :accessor label-color
		:type unicode
		:initform (cljw:unicode "")
		:metadata (:sync t
				 :json-name "label_color"))
   (grid-color :accessor grid-color
	       :type unicode
	       :initform (cljw:unicode "")
	       :metadata (:sync t
				:json-name "grid_color"))
   (color :accessor color
	  :type unicode
	  :initform (cljw:unicode "")
	  :metadata (:sync t
			   :json-name "color"))
   (label-offset :accessor label-offset
		 :type unicode
		 :initform (cljw:unicode "")
		 :metadata (:sync t
				  :json-name "label_offset"))
   (visible :accessor visible
	    :type bool
	    :initform :true
	    :metadata (:sync t
			     :json-name "visible"))
   (tick-style :accessor tick-style
	       :type list
	       :initform nil
	       :metadata (:sync t
				:json-name "tick_style")))
   (:default-initargs
       :view-name (cljw:unicode "Axis")
     :model-name (cljw:unicode "AxisModel")
     ;:ipython-display nil
     )

   (:metaclass traitlets:traitlet-class))

(defclass color-axis (axis)
  ((orientation :accessor orientation
		:type unicode
		:initform (cljw:unicode "horizontal")
		:metadata (:sync t
				 :json-name "orientation"))
   (side :initarg :side :accessor side
	 :type unicode
	 :initform (cljw:unicode "bottom")
	 :metadata (:sync t
			 :json-name "side"))
   (label-axes :accessor label-axes
	  :type unicode
	  :initform (cljw:unicode "")
	  :metadata (:sync t
			   :json-name "label"))
   (scale :accessor scale
	  :initform (make-instance 'scale)
	  :metadata (:sync t
			   :json-name "scale"
					;,@cljw:*widget-serialization*
			   )))
   (:default-initargs
       :view-name (cljw:unicode "ColorAxis")
     :model-name (cljw:unicode "ColorAxisModel"))

  (:metaclass traitlets:traitlet-class))
     
(defmethod widget-slot-value ((w widget) slot-name)
  (slot-value w slot-name))
