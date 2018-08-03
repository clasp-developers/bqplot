(in-package :bqplot)

(defclass tooltip (cljw:domwidget)
  ((fields :accessor fields
	   :type list
	   :initform nil
	   :metadata (:sync t
			    :json-name "fields"))
   (formats :accessor formats
	    :type list
	    :initform nil
	    :metadata (:sync t
			     :json-name "formats"))
   (show-labels :accessor show-labels
		:type cljw:bool
		:initform :true
		:metadata (:sync t
				 :json-name "show_labels"))
   (labels-tooltip :accessor labels-tooltip
	   :type list
	   :initform nil
	   :metadata (:sync t
                      :json-name "labels")))
  
   (:default-initargs
    :view-name (cljw:unicode "Tooltip")
     :model-name (cljw:unicode "TooltipModel")
     :view-module (cljw:unicode "bqplot")
     :model-module (cljw:unicode "bqplot")
     :view-module-version *frontend-version*
     :model-module-version *frontend-version*)
   (:metaclass traitlets:traitlet-class))
