(in-package :bqplot)

;;;This class technically had a @register decorator but I removed it 
(defclass toolbar (cljw:domwidget)
  ((figure-toolbar :accessor figure-toolbar
	   :initform (make-instance 'figure)
	   :metadata #.`(:sync t
			    :json-name "figure"
			   ; ,@cljw:*widget-serialization*))
   (%panning :accessor panning
	     :type bool
	     :initform :true
	     :metadata (:sync t
			      :json-name "_panning"))
   (%panzoom :accessor panzoom
	     :initform (make-instance 'pan-zoom)
	     :metadata #.`(:sync t
			      :json-name "_panzoom"
			      ;*widget serialization*)))
					;TODO: look into non translated variables
   (:default-initargs
    :view-name (cljw:unicode "Toolbar")
    :model-name (cljw:unicode "ToolbarModel")
    :view-module (cljw:unicode "bqplot")
    :model-module (cljw:unicode "bqplot")
    :view-module-version *frontend-version*
    :model-module-version *frontend-version*)

  (:metaclass traitlets:traitlet-class))

			      
	   
  
