(in-package :bqplot)

(defclass base-axis (cljw:widget)
  ()
   (:default-initargs
    :view-module (cljw:unicode "bqplot")
     :model-module (cljw:unicode "bqplot")
     :view-module-version *frontend-version*
     :model-module-version *frontend-version*)
   (:metaclass traitlets:traitlet-class))

(defclass axis (base-axis)
  ((icon :accessor icon
	 :type cljw:unicode
	 :initform (cljw:unicode "fa-arrows"))	 
   (orientation :initarg :orientation :accessor orientation
		:type cljw:unicode
		:initform (cljw:unicode "horizontal")
		:metadata (:sync t
				 :json-name "orientation"))   
   (side :initarg :side :accessor side
;	 :type cljw:unicode
	 :initform :null
	 :metadata (:sync t
			  :json-name "side"))
   (label-axes :accessor label-axes
	  :type cljw:unicode
	  :initform (cljw:unicode "")
	  :metadata (:sync t
			   :json-name "label"))
   (grid-lines :accessor grid-lines
	       :type cljw:unicode
	       :initform (cljw:unicode "solid")
	       :metadata (:sync t
				:json-name "grid_lines"))
   (tick-format :accessor tick-format
	;	:type cljw:unicode
		:initform :null
		:metadata (:sync t
				 :jsone-name "tick_format"))
   (scale :initarg :scale :accessor scale
	  :initform (make-instance 'scale)
	  :metadata (:sync t
			   :json-name "scale"
                                        ;FIXME ,@cljw:*widget-serialization*
                           ))
   (num-ticks :accessor num-ticks
	      :type integer
	      :initform :null
	      :metadata (:sync t
			       :json-name "num_ticks"))
   (tick-values :accessor tick-values
		:type list
		:initform #() ; (list (cons "type" :null)(cons "values" :null))
		:metadata #.`(:sync t
			      :json-name "tick_values"
			      ,@*array-serialization*
			      ))
   (offset :accessor offset
	   :type list
	   :initform nil
	   :metadata #.`(:sync t
			 :json-name "offset"
			 ,@*array-serialization*
			 ))
   (label-location :accessor label-location
		   :type cljw:unicode 
		   :initform (cljw:unicode "middle")
		   :metadata (:sync t
				    :json-name "label_location"))
   (label-color :accessor label-color
	;	:type cljw:unicode
		:initform :null
		:metadata (:sync t
				 :json-name "label_color"))
   (grid-color :accessor grid-color
	      ; :type cljw:unicode
	       :initform :null
	       :metadata (:sync t
				:json-name "grid_color"))
   (color :accessor color
	 ; :type cljw:unicode
	  :initform :null
	  :metadata (:sync t
			   :json-name "color"))
   (label-offset :accessor label-offset
		; :type cljw:unicode
		 :initform :null
		 :metadata (:sync t
				  :json-name "label_offset"))
   (visible :accessor visible
	    :type cljw:bool
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
		:type cljw:unicode
		:initform (cljw:unicode "horizontal")
		:metadata (:sync t
				 :json-name "orientation"))
   (side :initarg :side :accessor side
	 :type unicode
	 :initform (cljw:unicode "bottom")
	 :metadata (:sync t
			 :json-name "side"))
   (label-axes :accessor label-axes
	  :type cljw:unicode
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
     
(defmethod widget-slot-value ((w cljw:widget) slot-name)
  (slot-value w slot-name))


(defparameter *axis-types* (list (cons "bqplot.Axis" (find-class 'axis))
                               (cons "bqplot.ColorAxis" (find-class 'color-axis))))
