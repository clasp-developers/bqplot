(in-package :bqplot)

(defclass figure (cljw:domwidget)
  ((title-figure :initarg :title :accessor title-figure
          :type cljw:unicode
          :initform (cljw:unicode "")
          :metadata (:sync t
                           :json-name "title"))
                           ;;does display_name "Title" Matter??))
   (axes-figure :initarg :axes-figure :accessor axes-figure
         :type vector
         :initform #()
         :metadata (:sync t
                          :json-name "axes"
                                        ;,@cljw:*widget-serialization*
					))
   (axis-registry :initarg :axis-registry :accessor axis-registry
		  :type list
		  :initform nil)
   (marks :initarg :marks :accessor marks
          :type vector
          :initform #()
          :metadata (:sync t
                           :json-name "marks"
                                        ;,@cljw:*widget-serialization*
                           ))
   (interaction :initarg :interaction :accessor interaction
                :initform :null 
                :metadata (:sync t
                                 :json-name "interaction"
                                        ;,@cljw:*widget-serialization*
                                 ))
   (scale-x :initarg :scale-x :accessor scale-x
            :initform (make-instance 'Linear-Scale :min 0.0 :max 1.0 :allow-padding :false)
            :metadata (:sync t
                             :json-name "scale_x"
                                        ;,@cljw:*widget-serialization*
                             ))
   (scale-y :initarg :scale-y :accessor scale-y
            :initform (make-instance 'Linear-Scale :min 0.0 :max 1.0 :allow-padding :false)
            :metadata (:sync t
                             :json-name "scale_y"
                                        ;,@cljw:*widget-serialization*
                             ))
   (title-style :initarg :title-style :accessor title-style
                :type cljw:dict
                :initform nil 
                :metadata (:sync t
                                 :json-name "title_style"))
   (background-style :initarg :background-style :accessor background-style
                     :type cljw:dict
                     :initform nil
                     :metadata (:sync t
                                     :json-name "background_style"))
   (legend-style :initarg :legend-style :accessor legend-style
                 :type dict
                 :initform nil
                 :metadata (:sync t
                                  :json-name "legend_style"))
   (legend-text :initarg :legend-text :accessor legend-text
                :type dict
                :initform nil
                :metadata (:sync t
                                 :json-name "legend_text"))
   (layout :initarg :layout :accessor layout
           ;:initform (make-instance 'LayoutTraitType :kw (list (cons "min-width" "125px")))
	   :initform (make-instance 'cljw::layout :min-width "125px")
           :metadata (:sync t
                            :json-name "layout"
                                        ;,@cljw:*widget-serialization*
                            ))
   (min-aspect-ratio :initarg :min-aspect-ratio :accessor min-aspect-ratio
                     :type float
                     :initform 1.0
                     ;:validator %validate-min-aspect-ratio
                     :metadata (:sync t
                                      :json-name "min_aspect_ratio"))
   (max-aspect-ratio :initarg :max-aspect-ratio :accessor max-aspect-ratio
                     :type float
                     :initform 6.0
                     ;:validator %validate-max-aspect-ratio
                     :metadata (:sync t
                                      :json-name "max_aspect_ratio"))
   (fig-margin :initarg :fig-margin :accessor fig-margin
               :type dict
               :initform (list (cons "top" 60) (cons "bottom" 60) (cons "left" 60) (cons "right" 60))
               :metadata (:sync t
                                :json-name "fig_margin"))
   (padding-x :initarg :padding-x :accessor padding-x
              :type float
              :initform 0.0
              :metadata (:sync t
                               :json-name "padding_x"
                               :help "min 0.0, max 1.0"))
   (padding-y :initarg :padding-y :accessor padding-y
              :type float
              :initform 0.025
              :metadata (:sync t
                               :json-name "padding_y"
                               :help "min 0.0, max 1.0"))
   (legend-location :initarg :legend-location :accessor legend-location
		    :type cljw:unicode
		    :initform (cljw:unicode "top-right")
                    :metadata (:sync t
                                     :json-name "legend_location"))
   (animation-duration :initarg :animation-duration :accessor animation-duration
                       :type integer
                       :initform 0
                       :metadata (:sync t
                                  :json-name "animation_duration"))
                                        ;;;display-name "Animation duration"
   ;;;pyplot.py creates a pyplot slot on the fly... Adding it statically here.
   (pyplot :initarg :pyplot :accessor pyplot
	   :initform nil)
   )
  (:default-initargs
   :view-name (cljw:unicode "Figure")
    :model-name (cljw:unicode "FigureModel")
    :view-module (cljw:unicode "bqplot")
    :model-module (cljw:unicode "bqplot")
    :view-module-version *frontend-version*
    :model-module-version *frontend-version*)
  (:metaclass traitlets:traitlet-class))


;;;TODO: Figure out what the default decorator does
(defmethod %default-scale-x ((self figure))
  (make-instance 'LinearScale :min 0 :max 1 :allow-padding nil))

;;;TODO: Figure out what the default decorator does
(defmethod %default-scale-y ((self figure))
  (make-instance 'LinearScale :min 0 :max 1 :allow-padding nil))

(defmethod save-png ((self figure) &key (filename nil))
  (let ((msg (list (cons "type" "save_png"))))
    (when filename
      (push (cons "filename" filename) msg))
  ;(send self msg) 
  (values)))

#|
;;;@validate('min-aspect-ratio')
(defmethod %validate-min-aspect-ratio (object val)
  (if (> val (max-aspect-ratio self))
      (error "Trying to set min-aspect-ratio greater than max-aspect-ratio")
      val))

;;;@validate('max-aspect-ratio')
(defmethod %valid-max-aspect-ratio (object val)
  (if (< val (min-aspect-ratio self))
      (error "Trying to set max-aspect-ratio less than min-aspect ratio.")
      val))

|# ;;;All this was commented out because of an error compiling
   ;;;claiming that self was not a valid variable.



