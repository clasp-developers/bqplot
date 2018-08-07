(in-package :bqplot)

(defparameter %context (list (cons "figure" nil)
			     (cons "figure-registry" nil)
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

;(defun hashtable (data v)
					;(warn "How to try data[v]"))

(defun reset-context (&key &allow-other-keys)
    (defparameter %context (list (cons "figure" nil)
			     (cons "figure-registry" nil)
			     (cons "scales" nil)
			     (cons "scale_registry" nil)
			     (cons "last_mark" nil)
			     (cons "current_key" nil)))) 

(defun show (&key (key nil) (display-toolbar t))
  (let ((figure nil))
    (if key
	(setf figure (cdr (assoc key (cdr (assoc "figure-registry" %context :test #'string=)) :test #'string=)))
	(setf figure (cdr (assoc "1" (current-figure) :test #'string=))))
	;(setf figure (make-instance 'figure :title "Example")))
    (if display-toolbar
	(progn (unless (pyplot figure)
		 (setf (pyplot figure) (make-instance 'toolbar :figure figure)))
	       (make-instance 'cljw::vbox :children (vector figure (pyplot figure))))
	figure))) ;;;what's this display function?


(defun figure (&rest kwargs &key (key nil) (fig nil) &allow-other-keys)
  ;;;We don't want key and fig to actually be in the kwargs plist
  (remf kwargs :key)
  (remf kwargs :fig)
  ;;;Now begins the translation of python code.
  (let ((scales-arg (getf kwargs :scales)))
    (remf kwargs :scales)
    (if (assoc "current_key" %context :test #'string=)
	(setf (cdr (assoc "current_key" %context :test #'string=)) key)
	(setf %context (append %context (cons "current_key" key))))
    (if fig
	(progn
	  (setf (cdr (assoc "figure" %context :test #'string=)) (list fig))
	  (when key
	    (setf (nth key (cdr (assoc "figure-registry" %context :test #'string=))) fig))
	  (loop for arg in kwargs do
	       (unless (assoc arg (cdr (assoc "figure" %context :test #'string=)) :test #'string=)
		 (setf (cdr (assoc "figure" %context :test #'string=))(append (cdr (assoc "figure" %context :test #'string=))(cons arg (cdr (assoc arg kwargs :test #'string=))))))))
	(progn
	  (if (not key)
              (setf (cdr (assoc "figure" %context :test #'string=))(list (cons "1" (make-instance 'figure))));(make-instance 'figure)) ;kwargs are supposed to be added after the make-instance but doing so raised error
	      (progn
                (unless (assoc key (assoc "figure-registry" %context :test #'string=))
                  (unless (getf kwargs :title)
                    (push (concatenate 'string "Figure" " " key) kwargs)
                    (push :title kwargs))
                  (setf (cdr (assoc key (cdr (assoc "figure-registry" %context :test #'string=))))(list (cons "1" (make-instance 'figure)))))
                (setf (cdr (assoc "figure" %context :test #'string=)) (cdr (assoc key (cdr (assoc "figure-registry" %context :test #'string=)))))
                (warn "How to Add a slot for each argument in kwargs")
   ;;;(scales key :scales scales-arg)
		(loop for arg in kwargs do
		     (unless (assoc arg (cdr (assoc "figure" %context :test #'string=)) :test #'string=)
		       (setf (cdr (assoc "figure" %context :test #'string=))(append (cdr (assoc "figure" %context :test #'string=))(cons arg (cdr (assoc arg kwargs :test #'string=)))))))
		))))
    (cdr (assoc "figure" %context :test #'string=))))
        
(defun close (key)
  (let ((figure-registry (cdr (assoc "figure-registry" %context)))
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

;;;(defun %process-data (&rest kwarg-names &key &allow-other-keys)
  ;;;(warn "TODO: Make %process data"))

;TODO

(defun scales (&key (key nil) (scales nil))
  (let ((old-ctxt (assoc "scales" %context :test #'string=)))
    (if (not key)
	;No key is provided
	(setf (cdr (assoc "scales" %context :test #'string=))(cdr (assoc (%get-attribute-dimension scales) old-ctxt :test #'string=)))
	;A key is provided
	(unless (assoc "scale-registry" %context :test #'string=) ;how does one search for a key within scale registry
		 (setf (cdr (assoc "scale-registry" %context :test #'string=))(%get-attribute-dimension scales))))
    (setf (cdr (assoc "scales" %context :test #'string=)) (nth key (cdr (assoc "figure_registry" %context :test #'string=))))))

;TODO

(defun xlim (low high)
  (set-lim low high "x"))

(defun ylim (low high)
  (set-lim low high "y"))

(defun set-lim (low high name)
  (let ((scale (cdr (assoc (%get-attribute-dimension name) (cdr (assoc "scales" %context :test #'string=))))))
    (setf (min scale) low
	  (max scale) high)
  scale))

(defun axes (&rest kwargs &key (mark nil) (options nil) &allow-other-keys)
  ;;;Remove mark and options from kwargs
  (remf kwargs :mark)
  (remf kwargs :options)
  (unless mark
    (let ((new_mark (cdr (assoc "last_mark" axes%context :test #'string=))))
      (if new_mark
          (setf mark (cdr (assoc "last_mark" %context :test #'string=)))
          (return-from axes nil))))
  (let* ((fig (getf kwargs :figure (current-figure)))
        (scales (scales-mark mark))
        (fig-axes (axes-figure (cdr (assoc "1" fig :test #'string=))))
        (axes nil)
        (scale-metadata nil)
        (dimension nil)
        (axis-args nil)
        (axis nil)
        (key nil)
	(axis-type nil))
    (loop for name in scales
       do
	 ;missing the function that checks to see if the scale is even needed (if name not in mark.class_trait_names(scaled=True):)
         (setf name (car name)
	       scale-metadata (cdr (assoc name (scales-metadata (car mark)) :test #'string=))
               dimension (if (cdr (assoc "dimension" scale-metadata :test #'string=))(cdr (assoc "dimension" scale-metadata :test #'string=))(cdr (assoc (intern name "KEYWORD") scales :test #'string=)))
               axis-args (list scale-metadata))
               ;axis (%fetch-axis fig dimension (cdr (assoc name scales :test #'string=))) removing the ability to access an already existing set of axes from axis registry
         (warn "How to handle **(options.get(name, {}))")
         (if axis
             (progn
               ;(%apply-properties axis (getf options name nil)) THIS NEEDS TO WORK
               (push (cons name axis) axes))
             (progn
               (setf key (traitlets:traitlet-metadata 'mark (intern name "KEYWORD") :atype))
               (when key
                 (setf axis-type (cdr (assoc key (axis-types (make-instance 'axes) :test #'string=)))
                       axis (axis-type :scale (cdr (assoc name scales :test #'string=)) axis-args) ;;;How to handle **Axis_args
                       fig-axes (append fig-axes (list axis)))
                 (%update-fig-axis-registry fig dimension (cdr (assoc name scales :test #'string=)) axis)))))
    (setf (axes-figure (cdr (assoc "1" fig :test #'string=))) fig-axes)
    (print "Done with axes")
    axes))


(defun %set-label (label mark dim &rest kwargs &key &allow-other-keys)
  (unless mark
    (setf mark (cdr (assoc "last_mark" %context :test #'string=))))
  (unless mark
    (return-from %set-label nil))
  (let* ((fig (getf kwargs :figure))
	(scales (scales mark))
	(scales-metadata (getf (scales-metadata mark) dim))
	 (scale (getf scales dim)))
    (unless scale
      (return-from %set-label nil))
    (let* ((dimension (getf scales-metadata "dimension"))
	  (axis (%fetch-axis fig dimension (cdr (assoc dim scales)))))
      (unless dimension  
	(setf dimension (cdr (assoc dim scales))))
      (when axis
	(%apply-properties axis (cons "label" label)))))
  (values))

(defun xlabel (&rest kwargs &key (label nil) (mark nil) &allow-other-keys)
  (remf kwargs :label)
  (remf kwargs :mark)
  (%set-label (label mark "x" kwargs)))

(defun ylabel (&rest kwargs &key (label nil) (mark nil) &allow-other-keys)
  (remf kwargs :label)
  (remf kwargs :mark)
  (%set-label (label mark "y" kwargs)))


;;;for plot func pass in x and y as the parameres instead of the arg and kwarg 

;;;for plot func pass in x and y as the parameres instead of the arg and kwarg

(defun grids (&key (fig nil) (value "solid"))
  (unless fig
    (setf fig (current-figure)))
  (loop for a in (axes fig)
     do
       (setf (grid-lines a) value)))

(defun title (label &key (style nil) &allow-other-keys) ;no need for kwargs but apparently we're not allowed to say &rest &key
  (let ((fig (current-figure)))
    (setf (title fig) label)
    (when style
      (setf (title-style fig) style))))

(defun legend ()
  (loop for m in (marks (current-figure))
     do
       (setf (display-legend m) t)))

(defun hline (level &rest kwargs &key &allow-other-keys)
  (unless (getf kwargs :colors)
    (setf kwargs (append kwargs (list :colors "dodgerblue"))))
  (unless (getf kwargs :stroke-width)
    (setf kwargs (append kwargs (cons :stroke-width 1))))
  (let* ((scales (getf kwargs :scales))
	(fig (getf kwargs :figure))
	(x nil)
	(y nil)
	(scales-x (assoc "x" scales :test #'string=)))
    (unless fig
      (setf fig (current-figure)))
    (if scales-x
	(setf (cdr scales-x) (scale-x fig))
	(setf scales (append scales (cons "x" (scale-x fig)))))
    (remf kwargs :scales)
    (setf level (vector level))
    (if (= (length (array-dimensions level)) 0)
	(setf x (list 0 1)
	      y (list level level))
	(setf x (list 0 1)
	      ;;;y (column-stack (list level level))
	      )))
  ;(plot x y :scales scales :preserve-domain (list (cons "x" t)(cons "y" (getf kwargs :preserve-domain))) :axes nil :update-context nil kwargs)
  )
   
(defun vline (level &rest kwargs &key &allow-other-keys)
  (unless (getf kwargs :colors)
    (append kwargs (cons :colors "dodgerblue")))
   (unless (getf kwargs :stroke-width)
     (append kwargs (cons :stroke-width 1)))
   (let (x
	 y
	 (scales (getf kwargs :scales))
	 (fig (getf kwargs :figure (current-figure))))
     (setf (cdr (assoc "y" scales :test #'string=)) (scale-y fig))
     (remove ':scales kwargs)
     (remove scales kwargs)
     (setf level (vector level))
     (if (= (length (array-dimensions level)) 0)
	 (progn (setf x (cons level level))
		(setf y (cons 0 1)))
	 (progn ;(setf x (column-stack (cons level level)))
		(setf x (cons 0 1))))
   ;(plot x y scales: scales preserve-domain: (list (cons 'y: t)(cons 'x: (getf kwargs :preserve-domain))) axes: nil update-context: nil kwargs) giving error messages when we try to call the plot function
   ))

(defun %process-cmap (cmap)
  (let ((option nil))
    (if (stringp cmap)
	(setf option (append option (cons "scheme" cmap)))
	(if (listp cmap)
	    (setf option (append option (cons "colors" cmap)))
	    (error "`cmap` must be a string (name of a color scheme)
                         or a list of colors, but a value of {} was given")))
    option))

#|
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
      (setf (cdr (assoc "color" options :test #'string=)) (list (cons (getf options "color")(%process-cmap cmap)))))
    (loop for name in (class-trait-names mark-type) do
	 (let ((dimension (%get-attribute-dimension name mark-type)))
	   (when (assoc name kwargs :test #'string=)
	     (loop-finish))
	   (if (assoc name scales :test #'string=)
	       (when update-context
		 (setf (cdr (assoc dimension (cdr (assoc "scales" %context :test #'string=))) :test #'string=) (cdr (assoc name scales :test #'string=))))
	       (if (assoc dimension (cdr (assoc "scales" %context :test #'string=)) :test #'string=)
		   (setf (cdr (assoc name scales :test #'string=))(cdr (assoc dimension (cdr (assoc "scales" %context :test #'string=)) :test #'string=)))
		   (let* ((traitlet)
			  ;(rtype)
			  ;(dtype)
			  ;(compat-scale-types)
			  ;(sorted-scales)
			  ;(scales)))))
	   ))))      


	 ;(let ((scales-arg (getf kwargs :scales)))
    ;;Make getf an effective pop of the (:scales value)
    ;(remove ':scales kwargs)
    ;(remove scales-arg kwargs)))

(defun %infer-x-for-line (y)
  (let ((array-shape (array-dimensions y)))
       (when (= (length array-shape) 0) nil)
       (when (= (length array-shape) 1)
	 (array (cdr (assoc 0 array-shape :test #'equalp=))))
  pretty sure arange is a numpy function that we can't call because we haven't  
       (when (> (length array-shape) 1)
					(array (cdr (assoc 1 array-shape :test #'equalp=))))))
	   )))) |#

;;;In python, the lambda list is def _mark_type(mark_type, options={}, axes_options={}, **kwargs.
;;;I'm going to get rid of the option optionals and just have a kwargs containing all the information.


(defun %draw-mark (mark-type kwargs)
  (let ((fig (getf kwargs :figure (current-figure)))
        (scales (getf kwargs :scales))
        (update-context (getf kwargs :update-context t))
        (cmap (getf kwargs :cmap))
        (options (getf kwargs :options))
        (axes-options (getf kwargs :axes-options))
        (mark nil))
    (remf kwargs :fig)
    (remf kwargs :scales)
    (remf kwargs :cmap)
    (warn "Process color maps")
    #|
    (when cmap
      (if (assoc "color" options :test #'string=)
          (setf (cdr (assoc "color" options :test #'string=)) (list (cons 
    |#

    ;;;This loop is mimicking Python's mark_type.class_trait_names(scaled=True):
    (loop for (name . symbol)  in (list (cons "x" 'bqplot::x) (cons "y" 'bqplot::y) (cons "color" 'bqplot::color))
       do
         (let ((dimension (%get-attribute-dimension name (make-instance mark-type))))
           ;;;This cond is the entire body of the loop. It consists of 3 conditions, and then a final 'else':
           ;;;First, we check to see if name is not contained in kwargs.
           ;;;If it is not (it is likely that color will not be in kwargs, for instance), then we do nothing.
           ;;;If name is in kwargs, then we check to see if name is in scales (scales is the value of :scales in kwargs, or nil if not present
           ;;;If name is not in scales, then we check to see if dimension is not in %context['scales']. 
           (cond ((not (getf kwargs (intern name "KEYWORD")))
                  (values))
                 ((getf scales (intern name "KEYWORD"))
                  (when update-context
                    (setf (cdr (assoc dimension (cdr (assoc "scales" %context :test #'string=)) :test #'string=)) (cdr (assoc name scales :test #'string=)))))
                 ((not (assoc dimension (cdr (assoc "scales" %context :test #'string=)) :test #'string=))
                  (let* ((traitlet (symbol mark-type))
                         (rtype (traitlets:traitlet-metadata mark-type symbol :rtype))
                         ;;(dtype (validate the datatype is correct)
                         (dummy-scale (make-instance 'scale))
                         (compat-scale-types (loop for (str . instance) in (scale-types dummy-scale) when (string= rtype (rtype instance)) collect instance))
                         (sorted-scales (stable-sort compat-scale-types #'< :key #'precedence)))
                    (if (assoc name scales :test #'string=)
                        (setf (cdr (assoc name scales :test #'string=)) (last sorted-scales))
                        (push (cons name (last sorted-scales)) scales))
                    (when update-context
                      (if (assoc dimension (cdr (assoc "scales" %context :test #'string=)) :test #'string=)
                          (setf (cdr (assoc dimension (cdr (assoc "scales" %context :test #'string=)) :test #'string=)) (cdr (assoc name scales :test #'string)))
                          (push (cons dimension (cdr (assoc name scales :test #'string=))) (cdr (assoc "scales" %context :test #'string=)))))))
                 (t
                  (if (assoc name scales :test #'string=)
                      (setf (cdr (assoc name scales :test #'string=)) (cdr (assoc dimension (cdr (assoc "scales" %context :test #'string=)) :test #'string=)))
                      (push (cons name (cdr (assoc dimension (cdr (assoc "scales" %context :test #'string=)) :test #'string=))) scales))))))

    (setf kwargs (append kwargs (list :scales-mark scales))
          mark (apply #'make-instance mark-type kwargs))
    (if (assoc "last-mark" %context :test #'string=)
        (setf (cdr (assoc "last-mark" %context :test #'string=)) mark)
        (push (cons "last-mark" mark) %context))
    (setf (marks fig) (append (marks fig) (list mark)))
    (axes :mark mark :options axes-options)
    (print "Done with %draw-mark")
    mark))


#|


;;;In Python, x and y are optional arguments, but we're going to make them forced positional arguments here. If you want to call plot without x, use nil as the first argument, and we'll catch it here.
;;;Use keyword arguments, so we have a plist for kwargs
;;;In python, they give an 'index_data' as a key, and if x is not supplied, then this index_data key becomes x. That should not be relevant to us. 
(defun plot (x y &rest kwargs &key &allow-other-keys)
  (let ((marker-str (getf kwargs :marker-str)))
    (unless x
      (setf x (%infer-x-for-line y)))
    (setf kwargs (append kwargs (list :x x :y y)))
    (if  marker-str
         (progn
           (strip marker-str)
           (multiple-value-bind (line-style color marker) (%get-line-styles marker-str)
             (if (and marker (not line-style))
                 (progn
                   (when color
                     (setf kwargs (append kwargs (list :default-colors (list color)))))
                   (return-from plot (%draw-mark (find-class 'scatter) kwargs)))
                 (progn
                   (if line-style
                       (setf kwargs (append kwargs (list :line-style line-style)))
                       (setf kwargs (append kwargs (list :line-style "solid"))))
                   (when marker
                     (setf kwargs (append kwargs (list :marker marker))))
                   (when color
                     (setf kwargs (append kwargs (list :color (list color)))))
                   (return-from plot (%draw-mark (find-class 'lines) kwargs))))))
         (%draw-mark (find-class 'lines) kwargs))))
#|
(defun plot (&rest args)
 (let* ((x (if (keywordp (first args))
              (nil) ;return error message
             (first args))) ;x needs to be popped 
        (y (if (or (stringp (second args))(keywordp (second args)))
               x ;x needs to be redefined
	       (second args))) ;y needs to be popped
	(marker-str (when (and (not (keywordp (second args)))(or (stringp (second args))(stringp (third args))))
			(if (stringp (second args))
			    (second args)
			    (third args)))
	  ))
   ;popping everything out of args
   (when (equal x (first args))
     (remove (first args) args))
   (when (or (equal y (second args))(equal marker-str (second args)))
     (remove (second args) args))
   (when (equal marker-str (third args))
     (remove (third args) args))
   ;redefining x if x and y are the same
   (when (equal x y)
     (setf x (or (getf args "index-data" nil)
		 (%infer-x-for-line y))))  
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

|#

;;;Helper function for plot
(defun strip (string)
  (string-trim #(#\Space #\Newline #\Return) string))



|#

;;;In Python, x and y are optional arguments, but we're going to make them forced positional arguments here. If you want to call plot without x, use nil as the first argument, and we'll catch it here.
;;;Use keyword arguments, so we have a plist for kwargs
;;;In python, they give an 'index_data' as a key, and if x is not supplied, then this index_data key becomes x. That should not be relevant to us. 
(defun plot (x y &rest kwargs &key &allow-other-keys)
  (let ((marker-str (getf kwargs :marker-str)))
    (unless x
      (setf x (%infer-x-for-line y)))
    ;(setf kwargs (append kwargs (list :x x :y y)))
    (if  marker-str
         (progn
           (strip marker-str)
           (multiple-value-bind (line-style color marker) (%get-line-styles marker-str)
             (if (and marker (not line-style))
                 (progn
                   (when color
                     (setf kwargs (append kwargs (list :default-colors (list color)))))
                   (return-from plot (%draw-mark (find-class 'scatter) kwargs)))
                 (progn
                   (if line-style
                       (setf kwargs (append kwargs (list :line-style line-style)))
                       (setf kwargs (append kwargs (list :line-style "solid"))))
                   (when marker
                     (setf kwargs (append kwargs (list :marker marker))))
                   (when color
                     (setf kwargs (append kwargs (list :color (list color)))))
                   (return-from plot (%draw-mark (find-class 'lines) kwargs))))))
         (%draw-mark (find-class 'lines) kwargs))))

;;;Helper function for plot
(defun strip (string)
  (string-trim #(#\Space #\Newline #\Return) string))

(defun imshow (image format &rest kwargs &key &allow-other-key)
  (let ((ipyimage)(data))
    (if (equal format "widget")
	(setf ipyimage image)
	(if (equal format "filename")
	    (progn ;(setf data f.read)
	      (setf ipyimage (make-instance 'ipyimage :value data)))
	    (setf ipyimage (make-instance 'ipyimage :value image :format format))))
    (setf (cdr (assoc "image" kwargs :test #'string=)) ipyimage)))
					;^^^^INCOMPLETE^^^^^^
;(defun OHLC (args);; kwarg and args 
  ;(when (= (len args) 2)
    ;(setf kwargs (append kwargs (list :x (cdr (assoc 0 args :test #'equalp))))
	  ;kwargs (append kwargs (list :y (cdr (assoc 1 args :test #'equalp))))))
  ;(when (= (len args) 1)
    ;(setf kwargs (append kwargs (list :y (cdr (assoc 0 args :test #'equalp))))
          ;length (len (cdr (assoc 0 args :test #'equalp=))) 
	  ;kwargs (appends kwargs (list :x (array length))))) ;;need to change arange 
  ;(%draw-mark (OHLC kwargs)))

;;;OHCL needs to be completely recoded to take into account kwargs

(defun scatter (x y &rest kwargs &key &allow-other-keys)
  (setf kwargs (append kwargs (list :x x))
	kwargs (append kwargs (list :y y)))
  (print "scatter working")
  (%draw-mark (find-class 'scatter) kwargs)
  )

(defun hist (sample &rest kwargs &key (options nil)  &allow-other-key)
 (remf kwargs :option)
 (setf kwargs (append kwargs (list :sample sample)))
 (let ((scales (getf kwargs ':scales))(dimension))
   (remf kwargs ':scales)
draw-   (unless (member "count" scales)
     (setf dimension (%get-attribute-dimension "count" (find-class 'Hist)))
     (if (member dimension (cdr (assoc "scales" %context :test #'string=)))
	 (setf scales (append scales (list :count (nth dimension (cdr (assoc "scales" %context :test #'string=))))))
	 (progn (setf (cdr (assoc "count" scales :test #'string=)) (make-instance 'linear-scale (getf options "count")))
		(setf (nth dimension (cdr (assoc "scales" %context :test #'string=))) (cdr (assoc "count" scales :test #'string=))))))
 (setf (cdr (assoc "scales" kwargs :test #'string=)) scales))
 (%draw-mark (find-class 'Hist) :options options kwargs))

(defun bin (sample &rest kwargs &key (options nil) &allow-other-keys)
 (let ((scales)(dimension))
   (setf kwargs (append kwargs (list :sample sample))
      scales (cdr (assoc "scales" kwargs :test #'string=)))
   (remf kwargs :scales)
   (loop for xy in (list "x" "y") do
     (unless (member xy scales)
       (progn (setf dimension (%get-attribute-dimension xy (find-class 'bars)))
	      (if (member dimension (cdr (assoc "scales" %context :test #'string=)))
		  (setf (cdr (assoc xy scales :test #'string=)) (nth dimension (cdr (assoc "scales" %context :test #'string=))))
		  (progn (setf (cdr (assoc xy scales :test #'string=)) (linear-scale (getf options xy)))
			 (setf (nth dimension (cdr (assoc "scales" %context :test #'string=))) (cdr (assoc xy scales :test #'string=))))))))
   (setf kwargs (append kwargs (list :scales scales)))
   (%draw-mark (find-class 'bins) :options options kwargs)))


;;checked
(defun bar (x y &rest kwargs &key &allow-other-key)
  (setf kwargs (append kwargs (list :x x))
	kwargs (append kwargs (list :y y)))
  (%draw-mark (find-class 'bars) kwargs))

;;need to check the class boxplot 
(defun boxplot (x y &rest kwargs &key &allow-other-key)
  (setf kwargs (append kwargs (list :x x))
	kwargs (append kwargs (list :y y)))
  (%draw-mark (find-class 'boxplot) kwargs))

#|
(defun barh (arg kwargs) ;args and kwargs 
  (setf kwargs (append kwargs (list :orientation "horizontal")))
  (bar (args kwargs)));;arg kwargs 
|#

;;checked 
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
      (setf (cdr (assoc "projection" scales :test #'string=)) (make-instance 'mercator (getf options "projection"))))
    (setf (cdr (assoc "scales" kwargs :test #'string=)) scales)
    ;(if (isinstance map-data string-types)
	;(setf (cdr (assoc "map-data" kwargs :test #'string=)) (topo-load ));figure out how the string works
	;(setf (cdr (assoc "map-data" kwargs :test #'string=))(map-data)))
    )
    (%draw-mark (find-class 'map) kwargs))

;;checked 
(defun heat-map (color &rest kwargs &key &allow-other-key)
  (setf kwargs (append kwargs (list :color color)))
  (%draw-mark (find-class 'heat-map) kwargs))

;;checked
(defun grid-heat-map (color &rest kwargs &key &allow-other-key)
    (setf kwargs (append kwargs (list :color color)))
   (%draw-mark (find-class 'grid-heat-map) kwargs))

;(defun %add-interaction (int-type &rest kwargs &key &allow-other-keys)
  ;(let* ((fig (getf kwargs "figure" (current-figure)))
	 ;(marks (getf kwargs "marks")) ;[_context['last_mark']]
	 ;(dimension)
	 ;(interaction))
    ;(loop for name traitlet
	 ;(setf dimension (get-metadata traitlet "dimension")) ;probably wrong
	 ;(unless dimension
	   ;(setf (cdr (assoc name kwargs :test #'string=)) (%get-context-scale "dimension"))))
    ;(setf (cdr (assoc "marks" kwargs :test #'string=)) marks
	  ;interaction (int-type kwargs)
	  ;)
    ;(when (interaction fig) ;figure out how to translate this
      ;(fig.interaction.close())
      ;figure out how to translate this
    ;(setf (interaction fig) interaction) ;figure out what this fig.interaction is
  ;interaction))

;;checked 
(defun %get-context-scale (dimension)
  (nth dimension (cdr (assoc "scales" %context :test #'string=))))

(defun %create-selector (int-type func trait &rest kwargs &key &allow-other-keys)
  (let ((interaction (%add-interaction int-type kwargs)))
    ;(when func
      ;(on-trait-change interaction func trait))
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

(defun clear-figure ()
  (let ((fig (cdr (assoc "figure" %context :test #'string=))))
    (when fig 
      (setf (marks fig) nil
	    (axes fig) nil
	    ;("axis-registry" fig) nil) ;; did i handle setattr right?
	    (cdr (assoc "scales" %context :test #'string=)) nil)
      (let ((key (cdr (assoc "current-key" %context :test #'string=))))
	(when key
	  (setf  (cdr (assoc key (cdr (assoc "scale-registry" %context :test #'string=)))) nil ))))))

;;needs to be checked 
(defun current-figure ()
  (unless (cdr (assoc "figure" %context :test #'string=))
    (figure)) ;;is the is the proper way to call the func
  (cdr (assoc "figure" %context :test #'string=)))


;(defun get-context ())

;(defun set-context (context))


(defun %fetch-axis (fig dimension scale)
  (let* ((axis-registry (cdr (assoc  "axis-registry" fig :test #'string=)))
	 (dimension-data (getf axis-registry dimension nil))
	 (dimension-scales (loop for dim in dimension-data collect (cdr (assoc "scale" dim :test #'string=))))
	 (dimension-axes (loop for dim in dimension-data collect (cdr (assoc "axis" dim :test #'string=)))))
     ))

(defun %update-fig-axis-registry (fig dimension scale axis)
  (let* ((axis-registry (axis-registry fig))
	 (dimension-scales (cdr (assoc dimension axis-registry :test #'string=))))
    (setf dimension-scales (append dimension-scales (list (cons "scale" scale) (cons "axis" axis))))
    (if (cdr (assoc dimension axis-registry :test #'string=))
	(setf (cdr (assoc dimension axis-registry :test #'string=)) dimension-scales)
	(if axis-registry
	    (setf axis-registry (append axis-registry (cons dimension dimension-scales)))
	    (setf axis-registry (list (cons dimension dimension-scales)))))
    (setf (axis-registry fig) axis-registry)))
  
;(defun %get-attribute-dimension (trait-name &key (mark-type nil) &allow-other-keys)
  ;(unless mark-type
    ;return-from %get-attribute-dimension) ;;is this the right way to return this  
  ;(let scale-metadata ;;dont know the rest, for kevin 
      ;))

;(defun %apply-properties (widget  &key (properties nil) &allow-other-keys)
  ;(with-pathname-p (hold-sync widget) ;;not very clear on how this portion works 
    ;(loop for (key . value)  in (items properties)
       ;do
					;(setf (slot-value widget key) value))))

(defun %get-attribute-dimension (trait-name &optional mark-type)
  (unless mark-type
    (return-from %get-attribute-dimension trait-name))
  (let ((scale-metadata (scales-metadata mark-type)))
    (cdr (assoc "dimension" (cdr (assoc trait-name scale-metadata :test #'string=)) :test #'string=)))) 

(defun %get-line-styles (marker-str)
  (flet ((%extract-marker-value (marker-str code-dict) ;flet lets a fcn in a fcn 
	   (let ((val nil))
	     (block outer ;;mimics break 
	       (loop for code in code-dict
		  do
		    (when (search (car code) marker-str);;search for substring in string 
		      (setf val (cdr code))
		      (return-from outer nil))))
	     val)))
    (loop for code-dict in (list line-style-codes color-codes marker-codes)
       collect (%extract-marker-value marker-str code-dict))))

(defun %get-line-styles (marker-str)
  (defun %extract-marker-value (marker-str code-dict)
    (let (val)
      (loop for code in code-dict do
	   (when (member code marker-str)
		 (setf val (cdr (assoc code code-dict :test #'string=)))
					;something should break the loop right here
		 ))))
  (loop for code-dict in (list line-style-codes color-codes marker-codes) do
       (%extract-marker-value marker-str code-dict)
       ))

  

#|
(defmethod %ipython-display ((widget nglwidget) &rest key &key &allow-other-keys)
  (if (first-time-loaded widget)
      (setf (first-time-loaded widget) nil)
      (sync-view widget))
  (when (init-gui widget)
    (when (not (gui widget))
      (setf (gui widget) (%display (player widget))))
    (display (gui widget)))
  (when (or (string= "dark" (theme widget)) (string= "oceans16" (theme widget)))
    (warn "how do we set the theme")
    (%remote-call widget "cleanOutput" :target "Widget"))
  (%ipython-display (place-proxy widget))
  (values))

(defmethod display ((widget nglwidget) &key (gui nil) (use-box nil))
  (if gui
      (if use-box
          (let ((box (apply #'make-instance 'BoxNGL widget (%display (player widget)))))
            (setf (%gui-style box) "row")
             box)
          (progn
            (display widget)
            (display (%display (player widget)))
            (values)))
      widget))


(defmethod %display ((self trajectory-player))
  (let* ((box-factory (list
		       (cons (%make-general-box self) "General")
		       (cons (%make-widget-repr self) "Representation")
		       (cons (%make-widget-preference self) "Preference")
		       (cons (%make-theme-box self) "Theme")
		       (cons (%make-extra-box self) "Extra")
		       (cons (%make-hide-tab-with-place-proxy self) "Hide")
		       (cons (%show-website self) "Help")))
	 (tab (%make-delay-tab box-factory :selected-index 0)))
    (setf (align-self (layout tab)) "center" (align-items (layout tab)) "stretch")
    (setf (widget-tab self) tab)
    (widget-tab self)))

(defun %make-delay-tab (box-factory &optional (selected-index 0))
  (let ((tab (make-instance 'cl-jupyter-widgets::tab
			    :children (loop for (box) in box-factory
					 collect (make-instance 'cl-jupyter-widgets::Box))))
	(i 0))
    
    (loop for (dummy . title) in box-factory
       do
	 (set-title tab i title)
	 (incf i))

    (if (not (children (aref (children tab) selected-index)))
	(setf (selected-index tab) -1))

    (flet ((on-update-selected-index (change)
	     (let ((index (aref change "new")))
	       (if (not (children (aref (children tab) index)))
		   (setf (children (aref (children tab) index)) (error "I don't know what to set it to")))
	       )))
      (observe tab on-update-selected-index :names "selected-index")
      (setf (selected-index tab) selected-index)
      tab)))


|#


      
	  
      
