(in-package :bqplot)

(defclass scale (widget)
  ((scale-types :accessor scale-types
                :initform (list (cons "bqplot.mercator" (find-class 'mercator))
				(cons "bqplot.albers" (find-class 'albers))
				(cons "bqplot.albers-usa" (find-class 'albers-usa))
				(cons "bqplot.equi-rectangular" (find-class 'equi-rectangular))
				(cons "bqplot.orthographic" (find-class 'orthographic))
				(cons "bqplot.gnomonic" (find-class 'gnomonic))
				(cons "bqplot.stereographic" (find-class 'stereographic))
				(cons "bqplot.linear-scale" (find-class 'linear-scale))
				(cons "bqplot.log-scale" (find-class 'log-scale))
				(cons "bqplot.date-scale" (find-class 'date-scale))
				(cons "bqplot.ordinal-scale" (find-class 'ordinal-scale))
				(cons "bqplot.color-scale" (find-class 'color-scale))
				(cons "bqplot.date-color-scale" (find-class 'date-color-scale))
				(cons "bqplot.ordinal-color-scale" (find-class 'ordinal-color-scale))
				) ;;;TODO: Fill in class names
                )
   (precedence :accessor precedence
               :type integer
	       :initform 1)
   (domain-class :accessor domain-class
                 :type float
                 :initform nil)
   (reverse-scales :accessor reverse-scales
            :type bool
            :initform :false
            :metadata (:sync t
                             :json-name "reverse"))
   (allow-padding :accessor allow-padding
		  :type bool
		  :initform :true
		  :metadata (:sync t
				   :json-name "allow_padding")))
  (:default-initargs
   :view-name (cljw:unicode "Scale")
    :model-name (cljw:unicode "ScaleModel")
    :view-module (cljw:unicode "bqplot")
    :model-module (cljw:unicode "bqplot")
    :view-module-version *frontend-version*
    :model-module-version *frontend-version*
    ;:ipython-display nil
    )
  (:metaclass traitlets:traitlet-class))

(defclass geo-scale (scale)
  ()
  (:default-initargs
   :view-name (cljw:unicode "GeoScale")
    :model-name (cljw:unicode "GeoScaleModel"))
  (:metaclass traitlets:traitlet-class))

(defclass mercator (geo-scale)
  ((scale-factor :accessor scale-factor
		 :type float
		 :initform 190.0
		 :metadata (:sync t
				  :json-name "scale_factor"))
   (center :accessor center
	   :type list
	   :initform (list (cons 0 60)) 
	   :metadata (:sync t
			    :json-name "center"))
   (rotate :accessor rotate
	   :type list
	   :initform (list (cons 0 0))
	   :metadata (:sync t
			    :json-name "rotate"))
   (r-type :accessor r-type
	   :type unicode
	   :initform (cljw:unicode "(Number, Number)"))
   (d-type :accessor d-type
	    :type unicode
	   :initform (cljw:unicode "numpy.number")) ;;;;TODO kevin has no idea whats going on 
  )
  (:default-initargs
   :view-name (cljw:unicode "Mercator")
    :model-name (cljw:unicode "MercatorModel"))
  (:metaclass traitlets:traitlet-class))

(defclass albers (geo-scale)
  ((scale-factor :accessor  scale-factor
		 :type float
		 :initform 250.0
		 :metadata (:sync t
				  :json-name "scale_factor"))
    (rotate :accessor rotate
	   :type list
	   :initform (list (cons 96 0))
	   :metadata (:sync t
			    :json-name "rotate"))
   (center :accessor center
	   :type list
	   :initform (list (cons 0 60)) 
	   :metadata (:sync t
			    :json-name "center"))
   (parallels :accessor parallels
	   :type list
	   :initform (list  (cons 29.5 45.5))
	   :metadata (:sync t
			    :json-name "parallels"))
   (precision :accessor precision
	      :type float
	       :initform 0.1
	       :metadata (:sync t
				:json-name "precision"))
   (r-type :accessor r-type
	   :type unicode
	   :initform (cljw:unicode "(Number, Number)"))
   (d-type :accessor d-type
	    :type unicode
	   :initform (cljw:unicode "numpy.number")) ;;;;TODO kevin has no idea whats going on 
    )
  (:default-initargs
   :view-name (cljw:unicode "Albers")
    :model-name (cljw:unicode "AlbersModel"))
  
  (:metaclass traitlets:traitlet-class)) 

(defclass albers-usa (geo-scale)
  ((scale-factor :accessor  scale-factor
		 :type float
		 :initform 1200.0
		 :metadata (:sync t
				  :json-name "scale_factor"))
   (translate :accessor translate
		 :type list
		 :initform (list (cons 600 490))
		 :metadata (:sync t
				  :json-name "translate"))
  
   (r-type :accessor r-type
	   :type unicode
	   :initform (cljw:unicode "(Number, Number)"))
   (d-type :accessor d-type
	    :type unicode
	   :initform (cljw:unicode "numpy.number")) ;;;;TODO kevin has no idea whats going on 
   )
  
  (:default-initargs
    :view-name (cljw:unicode "AlbersUSA")
     :model-name (cljw:unicode "AlbersUSAModel"))
   (:metaclass traitlets:traitlet-class))


(defclass equi-rectangular (geo-scale)
   ((scale-factor :accessor scale-factor
		 :type float
		 :initform 145.0
		 :metadata (:sync t
				  :json-name "scale_factor"))
   (center :accessor center 
	   :type list
	   :initform (list (cons 0 60)) 
	   :metadata (:sync t
			    :json-name "center")) 
  (r-type :accessor r-type
	   :type unicode
	   :initform (cljw:unicode "(Number, Number)"))
  (d-type :accessor d-type
	    :type unicode
	   :initform (cljw:unicode "numpy.number"))) ;;;;TODO kevin has no idea whats going on 
   (:default-initargs
   :view-name (cljw:unicode "EquiRectangular")
     :model-name (cljw:unicode "EquiRectangularModel"))
   
   (:metaclass traitlets:traitlet-class))
   
  
(defclass orthographic (geo-scale)
   ((scale-factor :accessor scale-factor
		 :type float
		 :initform 145.0
		 :metadata (:sync t
				  :json-name "scale_factor"))
   (center :accessor center 
	   :type list
	   :initform (list (cons 0 60)) 
	   :metadata (:sync t
			    :json-name "center"))
   (rotate :accessor rotate
	   :type list
	   :initform (list (cons 0 0))
	   :metadata (:sync t
			    :json-name "rotate"))
   (clip-angle :accessor clip-angle
               :type float
	       :initform 90.0 
	       :validator validate-clip-angle 
	       :metadata (:sync t
	                        :json-name "clip_angle"))
   (precision :accessor precision
	      :type float
	      :initform 0.1
	      :metadata (:sync t
			       :json-name "precision"))
   (r-type :accessor r-type
	   :type unicode
	   :initform (cljw:unicode "(Number, Number)"))
   (d-type :accessor d-type
	   :type unicode
	   :initform (cljw:unicode "numpy.number"))) ;;;;TODO kevin has no idea whats going on 
  (:default-initargs
   :view-name (cljw:unicode "Orthographic")
   :model-name (cljw:unicode "OrthographicModel"))
  (:metaclass traitlets:traitlet-class))

  
(defun validate-clip-angle (object val)
  (if  (slot-boundp object 'clip-angle) 
	(cond ((> val 360.0) 360.0)
	      ((< val 0.0) 0.0)
	      (t val)))
      val)
			  
(defclass gnomonic (geo-scale)
  ((scale-factor :accessor scale-factor
		 :type float
		 :initform 145.0
		 :metadata (:sync t
				  :json-name "scale_factor"))
   (center :accessor center 
	   :type list
	   :initform (list (cons 0 60))
	   :metadata (:sync t
			    :json-name "center"))
   (clip-angle :accessor clip-angle
               :type float
	       :initform 89.999 
	       :validator validate-clip-angle
	       :metadata (:sync t
	                  :json-name "clip_angle"))
   (precision :accessor precision
	       :type float
	       :initform 0.1
	       :metadata (:sync t
				:json-name "precision"))
   (r-type :accessor r-type
	   :type unicode
	   :initform (cljw:unicode "(Number, Number)"))
   (d-type :accessor d-type
	    :type unicode
	   :initform (cljw:unicode "numpy.number"))) ;;;;TODO kevin has no idea whats going on 
 (:default-initargs
   :view-name (cljw:unicode "Gnomonic")
   :model-name (cljw:unicode "GnomonicModel"))
 
  (:metaclass traitlets:traitlet-class))


(defclass stereographic (geo-scale)
    ((scale-factor :accessor scale-factor
		 :type float
		 :initform 145.0
		 :metadata (:sync t
				  :json-name "scale_factor"))
   (center :accessor center 
	   :type list
	   :initform (list (cons 0 60)) 
	   :metadata (:sync t
			    :json-name "center"))
   (rotate :accessor rotate
	   :type list
	   :initform (list (cons 96 0 ))
	   :metadata (:sync t
			    :json-name "rotate"))
   (clip-angle :accessor clip-angle
               :type float
	       :initform 179.9999 
	       :validator validate-clip-angle
	       :metadata (:sync t
	                  :json-name "clip_angle"))
   (precision :accessor precision
	       :type float
	       :initform 0.1
	       :metadata (:sync t
				:json-name "precision"))
   (r-type :accessor r-type
	   :type unicode
	   :initform (cljw:unicode "(Number, Number)"))
   (d-type :accessor d-type
	    :type unicode
	   :initform (cljw:unicode "numpy.number"))) ;;;;TODO kevin has no idea whats going on
 (:default-initargs
   :view-name (cljw:unicode "Stereographic")
   :model-name (cljw:unicode "StereographicModel"))
 
  (:metaclass traitlets:traitlet-class))

(defclass linear-scale (scale)
  ((precedence :accessor precedence
               :type int
	       :initform 2)   
   (r-type :accessor r-type
	   :type unicode
	   :initform (cljw:unicode "Number"))
   (d-type :accessor d-type
	    :type unicode
	    :initform (cljw:unicode "numpy.number"))
   (min :accessor min
	:type float
	:initform 0.0              
	:metadata (:sync t
			 :json-name "min"))
   (max :accessor max
	:type float
	:initform 1.0              ;;;;tentative value for now not sure what it does 
	:metadata (:sync t
			 :json-name "max"))
   (stabilized :accessor stabilized
	       :type bool
	       :initform :false
	       :metadata (:sync t
				:json-name "stabilized"))
   (min-range :accessor min-range
	      :type float
	      :initform 0.6
	      :validator validate-min-range 
	      :metadata (:sync t
			       :json-name "min_range"))
   (mid-range :accessor mid-range
	      :type float
	      :initform 0.8
	      :validator validate-mid-range
	      :metadata (:sync t
			       :json-name "mid_range")))
   (:default-initargs
   :view-name (cljw:unicode "LinearScale")
     :model-name (cljw:unicode "LinearScaleModel"))
   
  (:metaclass traitlets:traitlet-class))

;;;;Validator for min-range making sure its between [0,1]
(defun validate-min-range (object val)
  (if (slot-boundp object 'min-range)
	 (cond ((>= val  1.0) 1.0)
	       ((<= val 0.0) 0.0)
	       (t val))
    val))
;;;;Validator for mid-range making sure its between [.1,1]
(defun validate-mid-range (object val)
  (if (slot-boundp object 'mid-range)
	 (cond ((>= val  1.0) 1.0)
	       ((<= val 0.1) 0.1)
	       (t val))
	 val))


(defclass log-scale (scale) 
   ((r-type :accessor r-type
	   :type unicode
	   :initform (cljw:unicode "Number"))
   (d-type :accessor d-type
	    :type unicode
	    :initform (cljw:unicode "numpy.number"))
   (min :accessor min
	:type float  
	:initform 0            
	:metadata (:sync t
			 :json-name "min"))
   (max :accessor max
	:type float
	:initform 1              ;;;;tentative 
	:metadata (:sync t
			 :json-name "max")))
 (:default-initargs
   :view-name (cljw:unicode "LogScale")
   :model-name (cljw:unicode "LogScaleModel"))
 
 (:metaclass traitlets:traitlet-class))


(defclass date-scale (scale)
  ((r-type :accessor r-type
	   :type unicode
	   :initform (cljw:unicode "Number"))
   (d-type :accessor d-type
	    :type unicode
	    :initform (cljw:unicode "numpy.datetime64"));;;;TODO is this right?
   (min :accessor min
	:type date                                 ;;;;Not sure what to put for type 
	:initform nil
	:metadata (:sync t
			 :json-name "min"))
   (max :accessor max
	:type date                                  ;;;Not sure what to put for type 
	:initform nil
	:metadata (:sync t
			 :json-name "max")))

 (:default-initargs
   :view-name (cljw:unicode "DataScale")
    :model-name (cljw:unicode "DataScaleModel"))
 (:metaclass traitlets:traitlet-class))

(defclass ordinal-scale (scale)
  ((r-type :accessor r-type
	   :type unicode
	   :initform (cljw:unicode "Number"))
   (d-type :accessor d-type
	    :type unicode
	    :initform (cljw:unicode "numpy.str"))
   (domain :accessor domain
	   :type list
	   :initform nil
	   :metadata (:sync t
			    :json-name "domain")))	    
 (:default-initargs
   :view-name (cljw:unicode "DataScale")
   :model-name (cljw:unicode "DataScaleModel"))
 
 (:metaclass traitlets:traitlet-class))

(defclass color-scale (scale)
  ((r-type :accessor r-type
	   :type unicode
	   :initform (cljw:unicode "Color"))
   (d-type :accessor d-type
	    :type unicode
	    :initform (cljw:unicode "numpy.number"))
   (scale-type :accessor scale-type
	       :type unicode
	       :initform (cljw:unicode "linear")
	       :metadata (:sync t
				:json-name "scale_type"))
   (colors :accessor colors
	   :type list
	   :initform nil ;;;; not sure how to deal with the trait = color part
	   :metadata (:sync t
			    :json-name "color"))
   (min :accessor min
	:type float        
	:initform 0.0
	:metadata (:sync t
			 :json-name "min"))
   (max :accessor max
	:type float
	:initform 1   
	:metadata (:sync t
			 :json-name "max"))
   (mid :accessor mid
	:type float
	:initform 0.5    ;;;;not sure if you can use this value. just using for now   
	:metadata (:sync t
			 :json-name "mid"))
   (scheme :accessor scheme
	   :type unicode
	   :initform (cljw:unicode "RdYlGn")
	   :metadata (:sync t
			    :json-name "scheme")))
    (:default-initargs
   :view-name (cljw:unicode "ColorScale")
      :model-name (cljw:unicode "ColorScaleModel"))
    
    (:metaclass traitlets:traitlet-class))

(defclass date-color-scale (color-scale)
  ((d-type :accessor d-type
	    :type unicode
	    :initform (cljw:unicode "numpy.datetime64")) 
   ;;;;not sure about the domain-class
   (domain-class :accessor domain-class
		 :type date
		 :initform nil) 
   (min :accessor min
	:type float        
	:initform 0
	:metadata (:sync t
			 :json-name "min"))
   (max :accessor max
	:type float             
	:initform 1 
	:metadata (:sync t
			 :json-name "max"))
   (mid :accessor mid 
	:type float           
	:initform .5     
	:metadata (:sync t
			 :json-name "mid")))
   (:default-initargs
   :view-name (cljw:unicode "DateColorScale")
     :model-name (cljw:unicode "DataColorScaleModel"))
   
   (:metaclass traitlets:traitlet-class))

(defclass ordinal-color-scale (color-scale)
  ((r-type :accessor r-type
	   :type unicode
	   :initform (cljw:unicode "Color"))
   (d-type :accessor d-type
	    :type unicode
	    :initform (cljw:unicode "numpy.str"))
   (domain :accessor domain
	   :type list         
	   :initform nil
	   :metadata (:sync t
			    :json-name "domain")))
    (:default-initargs
   :view-name (cljw:unicode "DateColorScale")
      :model-name (cljw:unicode "DataColorScaleModel"))
    
   (:metaclass traitlets:traitlet-class))
