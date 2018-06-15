(in-package :bqplot)

(defparameter %context (list (cons "figure" nil)
			     (cons "figure_registry" nil)
			     (cons "scales" nil)
			     (cons "scale_registry" nil)
			     (cons "last_mark" nil)
			     (cons "current_key" nil)))

(defparameter line-style-codes (list (cons ":" "dotted")
				     (cons "-." "dash_dotted")
				     (cons "--" "dashed")
				     (cons "-" "solid")))

(defparameter color-codes (list (cons "b" "blue")
				(cons "g" "green")
				(cons "r" "red")
				(cons "c" "cyan")
				(cons "m" "magenta")
				(cons "y" "yellow")
				(cons "k" "black")))

(defparameter marker-codes (list (cons "o" "circle")
				 (cons "v" "triangle-down")
				 (cons "^" "triangle-up")
				 (cons "s" "square")
				 (cons "d" "diamond")
				 (cons "+" "cross")))

(defun hashtable (data v)
  (warn "How to try data[v]"))

(defun show (&key (key nil) (display-toolbar t))
  (let ((figure nil))
    (if key
	(setf figure (nth key (cdr (assoc "figure_registry" %context :test #'string=))))
      (setf figure (current-figure)))
    (if display-toolbar
	(unless (pyplot figure)
	  (setf (pyplot figure) (make-instance 'toolbar :figure figure)))
	(display (make-instance 'vbox :children (vector figure (pyplot figure))))
      (display figure)))
  (values))

(defun figure (&rest kwargs &key (key nil) (fig nil) &allow-other-keys)
  ;;;We don't want key and fig to actually be in the kwargs plist
  (remf kwargs :key)
  (remf kwargs :fig)
  ;;;Now begins the translation of python code.
  (let ((scales-arg (getf kwargs :scales)))
    ;;Make getf an effective pop of the (:scales value)
    (remove ':scales kwargs)
    (remove scales-arg kwargs)
    (setf (cdr (assoc "current_key" %context :test #'string=)) key)
    (if fig
	(progn
	  (setf (cdr assoc "figure" %context :test #'string=) fig)
	  (when key
	    (setf (nth key (cdr (assoc "figure_registry" %context :test #'string=))) fig))
	  (loop for arg in kwargs));;;Python wants to add slots to the figure class...
      ;;;setattr(%context['figure'], arg, kwargs[arg])
	;;Else clause of the if fig
	(progn
	  (if (not key)
              (setf (cdr (assoc "figure" %context :test #'string=)) (apply #'make-instance 'figure kwargs))
              (progn
                (unless (assoc key (assoc "figure_registry" %context :test #'string=))
                  (unless (getf kwargs :title)
                    (push (concatenate 'string "Figure" " " key) kwargs)
                    (push :title kwargs))
                  (setf (cdr (assoc key (cdr (assoc "figure_registry" %context :test #'string=)))) fig)
                (setf (cdr (assoc "figure" %context :test #'string=)) (cdr (assoc key (cdr (assoc "figure_registry" %context :test #'string=)))))
                (warn "How to Add a slot for each argument in kwargs"))))))
    (scales key :scales scales-arg)
        (loop for arg in kwargs)
    #|
    if(getattr(%context['figure'], 'axist_registry', None) is None):
         setattr(%context['figure'], 'axis_registry', {})
    |#
    ;;;Return the figure in context.
    (cdr (assoc "figure" %context :test #'string=))))
        
(defun close (key)
  (let ((figure-registry (cdr (assoc "figure_registry" %context)))
        (fig nil))
    (unless (member key figure-registry)
      (return-from close))
    (when (eq (cdr (assoc "figure" %context :test #'string=)) (cdr (assoc key figure-registry)))
      (figure))
    (setf fig (cdr (assoc key figure-registry)))
    ;;;if hasattr(fig, 'pyplot')
         ;;;fig.pyplot.close()
    ;;;del figure_registry[key]
    ;;;del _context['scale_registry'][key]
    (values)))

(defun %process-data (&rest kwarg-names &key &allow-other-keys)
  (warn "TODO: Make %process data"))

;TODO

(defun scales (&key (key nil) (scales nil))
  (let ((old-ctxt (assoc "scales" %context :test #'string=)))
    (if (not key)
	;No key is provided
	(setf (cdr (assoc "scales" %context :test #'string=)) ({_get_attribute_dimension(k):))
	;A key is provided
	(progn (unless (assoc "scale-registry" %context :test #'string=)) ;how does one search for a key within scale registry
		 (setf (cdr (assoc "scale-registry" %context :test #'string=)) ( _get_attribute_dimension(k):)))
	       (setf (cdr (assoc "scales" %context :test #'string=)) (nth key (cdr (assoc "figure_registry" %context :test #'string=))))))))

;TODO

(defun xlim (low high)
  (set-lim low high "x"))

(defun ylim (low high)
  (set-lim low high "y"))

(defun set-lim (low high name)
  (let ((scale (cdr (assoc (%get-attribute-dimension name) (cdr (assoc "scales" %context :test #'string=))))))
    (setf (min scale) low) (max scale) high)
  scale)

(defun axes (&rest kwargs &key (mark nil) (options nil) &allow-other-keys)
  ;;;Remove mark and options from kwargs
  (setf kwargs (remove mark kwargs)
        kwargs (remove ':mark kwargs)
        kwargs (remove options kwargs)
        kwargs (remove ':options kwargs))
  (unless mark
    (let ((new_mark (cdr (assoc "last_mark" %context :test #'string=))))
      (if new_mark
          (setf mark (cdr (assoc "last_mark" %context :test #'string=)))
          (return-from axes nil))))
  (let ((fig (getf :figure kwargs)))
    (unless fig
      (setf fig (current-figure)))
    (let ((scales (scales mark))
          (fig-axes (loop for axis in (axes fig) collect axis))
          (axes nil))
      )))
;;;FINISH AXES
    
(defun %set-label (label mark dim &rest kwargs &key &allow-other-keys)
  (unless (or mark (cdr (assoc "last_mark" %context :test #'string=)))
    (return-from %set-label nil))
  (unless mark
    (setf mark (cdr (assoc "last_mark" %context :test #'string=))))
  (let ((fig nil)
        (fig-val (getf :figure kwargs))
        (scales (scales mark))
        (scale-metadata nil)
        (scale-metadata-val (getf dim (scales-metadata mark)))
        (scale nil))
    (if fig-val
        (setf fig fig-val)
        (setf fig (current-figure)))
    (when scale-metadata-val
      (setf scale-metadata scale-metadata-val))
    (if (getf dim scales)
        (setf scale (getf dim scales))
        (return-from %set-label))
    (let ((dimension nil)
          (val (getf :dimension scale-metadata))
          (axis nil))
      (if val
          (setf dimension val)
          (setf dimension (cdr (assoc dim scales :test #'string=))))
      (setf axis (%fetch-axis fig dimension (cdr (assoc dim scales :test #'string=))))
      (when axis
        (%apply-properties axis (list (cons "label" label))))))
  (values))


;;;for plot func pass in x and y as the parameres instead of the arg and kwarg

(defun xlabel (&rest kwargs &key (label nil) (mark nil) &allow-other-keys)
  (setf kwargs (remove label kwargs)
        kwargs (remove ':label kwargs)
        kwargs (remove mark kwargs)
        kwargs (remove ':mark kwargs))
  (%set-label label mark "x" kwargs))

(defun ylabel (&rest kwargs &key (label nil) (mark nil) &allow-other-keys)
  (setf kwargs (remove label kwargs)
        kwargs (remove ':label kwargs)
        kwargs (remove mark kwargs)
        kwargs (remove ':mark kwargs))
  (%set-label label mark "x" kwargs))

(defun grids (&key (fig nil) (value "solid"))
  (unless fig
    (setf fig (current-figure)))
  (loop for a in (axes fig)
       (setf (grid_lines a) value)))
	

(defun title (label &rest &key (style nil) &allow-other-keys)
  (let (fig (current-figure))
  (setf (title fig) label)
  (unless style (setf (title_style fig) style))))

(defun legend ()
  (loop for m in (marks current-figure)
       (setf (display-legend m) t)))

(defun hline (level &rest kwargs &key &allow-other-keys)
  (unless (getf kwargs :colors)
    (setf kwargs (append kwargs (list :colors "dodgerblue")))
   (unless (getf kwargs :stroke-width)
     (setf kwargs (append kwargs (cons :stroke-width 1))))
   (let ((scales (getf kwargs :scales))
	  (fig (getf kwargs :figure))
	  (x nil)
	 (y nil)
	 (scales-x (assoc "x" scales :test #'string=)))
     (unless fig
       (setf fig (current-figure)))
     (if scales-x
	 (setf (cdr scales-x) (scale-x fig))
	 (setf scales (append scales (cons "x" (scale-x fig))))
   (remf kwargs :scales)
   (setf level (vector level))
   (if (= (length (shape level)) 0)
       (setf x (list 0 1)
	     y (list level level))
       (setf x (list 0 1)
	     y (column-stack (list level level)))))
   (plot x y :scales scales :preserve-domain (list (cons "x" t)(cons "y" (getf kwargs :preserve-domain))) :axes nil :update-context nil kwargs)))
   
(defun vline (level &rest kwargs &key &allow-other-keys)
  (unless (getf kwargs :colors)
    (append kwargs (cons :colors "dodgerblue")))
   (unless (getf kwargs :stroke-width)
     (append kwargs (cons :stroke-width 1)))
   (let* ((scales (getf kwargs :scales))
	  (fig (getf kwargs :figure current-figure))
	  ((cdr (assoc "y" scales :test #'string=))) (scale-y fig))
   (remove ':scales kwargs)
   (remove scales kwargs)
   (setf level (vector level))
   (if (= (length (shape level)) 0)
       (progn (setf x (cons level level))
	      (setf y (cons 0 1)))
       (progn (setf x (column-stack (cons level level)))
	      (setf x (cons 0 1))))
   (plot x y scales: scales preserve-domain: (list (cons 'y: t)(cons 'x: (getf kwargs :preserve-domain))) axes: nil update-context: nil kwargs)))

(defun %process-cmap (cmap)
  (let ((option nil))
    (if (stringp cmap)
	(setf (cdr (assoc "scheme" option :test #'string=)) cmap)
	(if (listp cmap)
	    (setf (cdr (assoc "colors" option :test #'string=)) cmap)
	    (error "`cmap` must be a string (name of a color scheme)
                         or a list of colors, but a value of {} was given")))
    option))

(defun %draw-mark (mark-type &rest kwargs &key (options nil) (axes-options nil) &allow-other-keys)
  (setf kwargs (remove options kwargs)
        kwargs (remove :options kwargs)
        kwargs (remove axes-options kwargs)
        kwargs (remove :axes-options kwargs))
  (let ((fig (getf kwargs :figure (current-figure)))
	(scales (getf kwargs :scales))
	(update-context (getf kwargs :update-context t))
	(cmap (getf kwargs :cmap)))
    (remf kwargs :figure)
    (remf kwargs :scales)
    (remf kwargs :update-context)
    (remf kwargs :cmap)
    (when cmap
      (setf (cdr (assoc "color" options :test #'string=)) (list (cons (getf options "color")))))
    (loop for name is 
      


	 (let ((scales-arg (getf kwargs :scales)))
    ;;Make getf an effective pop of the (:scales value)
    (remove ':scales kwargs)
    (remove scales-arg kwargs)))))

(defun %infer-x-for-line (y)
  (let array-shape (shape y)
       (when (= (length array-shape) 0) nil)
       (when (= (length array-shape) 1) (arange (cdr (assoc 0 array-shape :test #'equalp=))))
  ;pretty sure arange is a numpy function that we can't call because we haven't  
       (when (> (length array-shape) 1) (arange (cdr (assoc 1 array-shape :test #'equalp=))))))

(defun plot (&rest args)
 (let* ((x (if (keywordp (first args))
               ;return error message
             (first args))) ;x needs to be popped 
        (y (if (or (second args = string)(keywordp (second args)))
               x ;x needs to be redefined
	       (second args))) ;y needs to be popped
	(marker_str (when (and (not (keywordp (second args)))(or (second args = string)(third args = string)))
			(if (second args = string)
			    (second args)
			    (third args)))
	  ))
   ;popping everything out of args
   (when (equal x (first args))
     (remove (first args) args))
   (when (or (equal y (second args)(equal marker_str (second args))))
     (remove (second args) args))
   (when (equal marker_str (third args))
     (remove (third args) args))
   ;redefining x if x and y are the same
   (when (equal x y)
     (setf x (or (getf args "index-data" nil)
		 (%infer-x-for-line(y)))))  
     )
   
        ;; You could do more of this for non-keyword arguments
        )
  ; (destructuring-bind (&key foo bar baz) args
     ;; Do stuff with the values of x,y,foo,bar,baz
     ;; cases:
     ;; 1. x and y may be NIL - what does that even mean?
     ;; 2. x is something but y is NIL (for bar charts?)
     ;; 3. x and y are something
     ;;
           ;  )

(defun imshow (image format &rest kwargs &key &allow-other-key)
  (let ((ipyimage)(data))
  (if (equal format "widget")
      (setf ipyimage image)
      (if (equal format "filename")
	  (progn (setf data f.read)
		 (setf ipyimage (make-instance 'ipy-image :value data)))
	  (progn (setf ipyimage (ipy-image (setf value image)(setf format format))))))))
					;^^^^INCOMPLETE^^^^^^

(defun scatter (x y &rest kwargs &key &allow-other-keys)
  (setf kwargs (append kwargs (list :x x))
	kwargs (append kwargs (list :y y)))
  (%draw-mark (find-class 'scatter) kwargs))

(defun hist (sample options &rest kwargs &key &allow-other-key)
  (setf kwargs (append kwargs (list :sample sample)))
  (let ((scales (getf kwargs :scales))(dimension))
    (remf kwargs :scales)
    (unless (member "count" scales)
      (setf dimension (%get-attribute-dimension ("count" (find-class 'Hist)))
	      (if (member dimension (cdr (assoc "scales" %context :test #'string=)))
		  (setf scales (append scales (list :count (nth dimension (cdr (assoc "scales" %context :test #'string=))))))
		  (progn (setf (cdr (assoc "count" scales :test #'string=)) (linear-scale (getf options "count")))
			 (setf (nth dimension (cdr (assoc "scales" %context :test #'string=))) (cdr (assoc "count" scales :test #'string=)))))))
		  (setf (cdr (assoc "scales" kwargs :test #'string=)) scales)
		  (%draw-mark (find-class 'Hist) :options options kwargs)))

(defun bin (sample options &rest kwargs &key &allow-other-keys)
  (let ((scales)(dimension))
    (setf kwargs (append kwargs (list (:sample sample)))
	  scales (cdr (assoc "scales" kwargs :test #'string=)))
    (remf kwargs :scales)
    (loop for xy in (list "x" "y")
	 (unless (member xy scales)
	   (progn (setf dimension (%get-attribute-dimension xy (find-class 'bars)))
		  (if (member dimension (cdr (assoc "scales" %context :test #'string=)))
		      (setf (cdr (assoc xy scales :test #'string=)) (nth dimension (cdr (assoc "scales" %context :test #'string=))))
		      (progn (setf (cdr (assoc xy scales :test #'string=)) (linear-scale (getf options xy)))
			     (setf (nth dimension (cdr (assoc "scales" %context :test #'string=))) (cdr (assoc xy scales :test #'string=))))))))
    (setf kwargs (append kwargs (list (:scales scales))))
    (%draw-mark (find-class 'bins) :options options kwargs)))

(defun bar (x y &rest kwargs &key &allow-other-key kwargs)
  (setf kwargs (append kwargs (list :x x))
	kwargs (append kwargs (list :y y)))
  (%draw-mark (find-class 'bars) kwargs))

(defun boxplot (x y &rest kwargs &key &allow-other-key)
  (setf kwargs (append kwargs (list :x x))
	kwargs (append kwargs (list :y y)))
  (%draw-mark (find-class 'boxplot) kwargs))

(defun pie (sizes &rest kwargs &key &allow-other-key)
   (setf kwargs (append kwargs (list :sizes sizes)))
  (%draw-mark (find-class 'pie)  kwargs))
  
(defun label (text &rest kwargs &key &allow-other-key)
  (setf kwargs (append kwargs (list :text text)))
  (%draw-mark (find-class 'label) kwargs))

(defun geo (map-data &rest kwargs &key &allow-other-key)
  (let ((scales (getf kwargs :scales (cdr (assoc "scales" %context :test #'string=))))
	(options (getf kwargs :options)))
    (unless (member "projections" scales)
      (setf (cdr (assoc "projection" scales :test #'string=)) (mercator (getf options "projection"))))
    (setf (cdr (assoc "scales" kwargs :test #'string=)))
    (if (isinstance map-data string-types)
	(setf (cdr (assoc "map-data" kwargs :test #'string=)) (topo-load );figure out how the string works
	      (cdr (assoc "map-data" kwargs :test #'string=))(map-data)))
    (%draw-marks (find-class map) kwargs)))

(defun heat-map (color &rest kwargs &key &allow-other-key)
  (setf kwargs (append kwargs (list :color color)))
  (%draw-mark (find-class 'heat-map)  kwargs))

(defun grid-heat-map (color &rest kwargs &key &allow-other-key)
    (setf kwargs (append kwargs (list :color color)))
    (%draw-mark (find-class 'grid-heat-map) kwargs))

(defun %add-interaction (int-type &rest kwargs &key &allow-other-keys)
  (let ((fig (getf kwargs "figure" (current-figure)))
	(marks (getf kwargs "marks")) ;[_context['last_mark']]
	(dimension)
	(interaction))
    (loop for
	 (setf dimension (get-metadata traitlet "dimension")) ;probably wrong
	 (unless dimension
	   (setf (cdr (assoc name kwargs :test #'string=)) (%get-context-scale "dimension"))))
    (setf (cdr (assoc "marks" kwargs :test #'string=)) marks
	  interaction (int-type kwargs))
    (when fig.interaction ;figure out how to translate this
      fig.interaction.close()) ;figure out how to translate this
    (setf fig.interaction interaction)) ;figure out what this fig.interaction is
  interaction)

(defun %get-context-scale (dimension)
  (nth dimension (cdr (assoc "scales" %context :test #'string=))))

(defun %create-selector (int-type func trait &rest kwargs &key &allow-other-keys)
  (let (interaction (%add-interaction int-type kwargs))
    (when func
      (on-trait-change interaction func trait))
    interaction))

(defun brush-int-selector (&rest kwargs &key (func nil) (trait "selected") &allow-other-key)
  (remf kwargs :func)
  (remf kwargs :trait)
  (%create-selector (find-class 'brush-interval-selector) func trait kwargs))

(defun %int-selector (&rest kwargs &key (func nil) (trait "selected") &allow-other-key)
  (remf kwargs :func)
  (remf kwargs :trait)
  (%create-selector (find-class 'fast-interval-selector) func trait kwargs))

(defun %index-selector (&rest kwargs &key (func nil) (trait "selected") &allow-other-key)
  (remf kwargs :func)
  (remf kwargs :trait)
  (%create-selector (find-class 'index-selector) func trait kwargs))

(defun brush-selector (&rest kwargs &key (func nil) (trait "selected") &allow-other-key)
  (remf kwargs :func)
  (remf kwargs :trait)
  (%create-selector (find-class 'brush-selector) func trait kwargs))
  
(defun multi-selector (&rest kwargs &key (func nil) (trait "selected") &allow-other-key)
  (remf kwargs :func)
  (remf kwargs :trait)
  (%create-selector (find-class 'multi-selector) func trait kwargs))

(defun lasso-selector (&rest kwargs &key (func nil) (trait "selected") &allow-other-key)
  (remf kwargs :func)
  (remf kwargs :trait)
  (%create-selector (find-class 'lasso-selector) func trait kwargs))

(defun clear
    (let ))




(defun %apply-properties (widget  &key (properties nil) &allow-other-keys)
 (with-pathname-p (hold-sync widget) ;;not very clear on how this portion works
   (loop for (key . value)  in (items properties)
      do
	(setf (key widget) value))))

(defun %get-line-styles (marker-str)
  (defun %extract-marker-value (marker-str code-dict)
    (let (val)
      (loop for code in code-dict
	   (when code in marker-str
		 (setf val (cdr (assoc code code-dict :test #'string=)))
					;something should break the loop right here
		 ))))
  (loop for code-dict in (list line-style-code color-code marker-codes)
       (%extract-marker-value marker-str code-dict)
       ))

  


  



      
	  
      
