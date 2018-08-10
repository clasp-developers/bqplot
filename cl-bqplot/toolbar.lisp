(in-package :bqplot)


;; FIXME -widget-register
(defclass toolbar (cljw:domwidget)
  ((toolbar-figure :accessor toolbar-figure
	   :initform (make-instance 'figure)
	   :metadata (:sync t
			       :json-name "figure"
			       ;;,@*widget-serialization*
                               ))
   (%panning :accessor panning
	     :type bool
	     :initform :true
	     :metadata (:sync t
			      :json-name "_panning"))
   (%panzoom :accessor panzoom
	     :initform (make-instance 'pan-zoom)
	     :metadata (:sync t
			      :json-name "_panzoom"
			      ;; FIXME *widget serialization*
			      )))
  ;;TODO: look into non translated variables
  (:default-initargs
   :view-name (cljw:unicode "Toolbar")
    :model-name (cljw:unicode "ToolbarModel")
    :view-module (cljw:unicode "bqplot")
    :model-module (cljw:unicode "bqplot")
    :view-module-version *frontend-version*
    :model-module-version *frontend-version*)
  (:metaclass traitlets:traitlet-class))
