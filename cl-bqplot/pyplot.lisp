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

;(defun hashtable (data v)
					;(warn "How to try data[v]"))

(defun reset-context ()
    (setf %context (list (cons "figure" nil)
			     (cons "figure_registry" nil)
			     (cons "scales" nil)
			     (cons "scale_registry" nil)
			     (cons "last_mark" nil)
			     (cons "current_key" nil)))) 

(defun show (&key (key nil) (display-toolbar t))
  (let ((figure nil))
    (if key
	(setf figure (cdr (assoc key (cdr (assoc "figure_registry" %context :test #'string=)) :test #'string=)))
	(setf figure (cdr (assoc "1" (current-figure) :test #'string=))))
	;(setf figure (make-instance 'figure :title "Example")))
    (if display-toolbar
	(progn (unless (pyplot figure)
		 (setf (pyplot figure) (make-instance 'toolbar :figure figure)))
	       (make-instance 'cljw::vbox :children (vector figure (pyplot figure))))
	figure))) ;;;what's this display function?


(defun figure (&rest kwargs &key (key nil) (fig nil) &allow-other-keys)
  ;;;We don't want key and fig to actually be in the kwargs plist
  (print "Entered figure function")
  (remf kwargs :key)
  (remf kwargs :fig)
  ;(print "About to enter first let")
  ;;;Now begins the translation of python code.
  (let ((scales-arg (getf kwargs :scales)))
    (remf kwargs :scales)
    ;(print "checking for current_key in context")
    (if (assoc "current_key" %context :test #'string=)
	(setf (cdr (assoc "current_key" %context :test #'string=)) key)
	(setf %context (append %context (cons "current_key" key))))
    ;(format t "Checking fig... ~% FIG: ~a~%" fig)
    (if fig
	(progn
	  (setf (cdr (assoc "figure" %context :test #'string=)) (list fig))
	  (when key
	    (setf (nth key (cdr (assoc "figure_registry" %context :test #'string=))) fig))
	  (loop for arg in kwargs do
	       (unless (assoc arg (cdr (assoc "figure" %context :test #'string=)) :test #'string=)
		 (setf (cdr (assoc "figure" %context :test #'string=))(append (cdr (assoc "figure" %context :test #'string=))(cons arg (cdr (assoc arg kwargs :test #'string=))))))))
	(progn
          ;(print "In progn sequence where fig is NIL")
          ;(format t "KEY: ~a ~%" key)
	  (if (not key)
              (progn
                ;(print "in progn where key is NIL")
                (setf (cdr (assoc "figure" %context :test #'string=)) (make-instance 'figure)))
	      (progn
                ;(print "In progn where key is not nil")
                (unless (assoc key (assoc "figure_registry" %context :test #'string=))
                  (unless (getf kwargs :title)
                    (push (concatenate 'string "Figure" " " key) kwargs)
                    (push :title kwargs))
                  (setf (cdr (assoc key (cdr (assoc "figure_registry" %context :test #'string=))))(list (cons key (make-instance 'figure)))))
                (setf (cdr (assoc "figure" %context :test #'string=)) (cdr (assoc key (cdr (assoc "figure_registry" %context :test #'string=)))))
                ;(warn "How to Add a slot for each argument in kwargs")
   ;;;(scales key :scales scales-arg)
		(loop for arg in kwargs do
		     (unless (assoc arg (cdr (assoc "figure" %context :test #'string=)) :test #'string=)
		       (setf (cdr (assoc "figure" %context :test #'string=))(append (cdr (assoc "figure" %context :test #'string=))(cons arg (cdr (assoc arg kwargs :test #'string=)))))))
		))))
    ;(print "After fig IF statement, about to return (cdr (assoc \"figure\" %context :test #'string=")
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

;;;(defun %process-data (&rest kwarg-names &key &allow-other-keys)
  ;;;(warn "TODO: Make %process data"))

;TODO

(defun scales (&key (key nil) (scales nil))
  (let ((old-ctxt (assoc "scales" %context :test #'string=)))
    (if (not key)
	;No key is provided
	(setf (cdr (assoc "scales" %context :test #'string=))(cdr (assoc (%get-attribute-dimension scales) old-ctxt :test #'string=)))
	;A key is provided
	(unless (assoc "scale_registry" %context :test #'string=) ;how does one search for a key within scale registry
		 (setf (cdr (assoc "scale_registry" %context :test #'string=))(%get-attribute-dimension scales))))
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
  (print "In axes")
  ;;;Remove mark and options from kwargs
  (remf kwargs :mark)
  (remf kwargs :options)
  ;(format t "Inside axes. kwargs is ~a~% mark is ~a~% options is ~a~%" kwargs mark options)
  (unless mark
    (let ((new_mark (cdr (assoc "last_mark" %context :test #'string=))))
      (if new_mark
          (setf mark (cdr (assoc "last_mark" %context :test #'string=)))
          (return-from axes nil))))
  (let* ((fig (getf kwargs :figure (current-figure)))
         (scales (scales-mark mark))
         (fig-axes (loop for axes-instance across (axes-figure fig) collect axes-instance))
         (axes nil)
         (scale-metadata nil)
         (dimension nil)
         (axis-args nil)
         (axis nil)
         (key nil)
         (axis-type nil))
    ;(format t "Outside current-figure~%. About to enter loop, scales is ~a~%" scales)
    (loop for (name . instance) in scales
       do
       ;;missing the function that checks to see if the scale is even needed (if name not in mark.class_trait_names(scaled=True):)
       ;;mark.class_trait_names(scaled=True) returns a list of all slots that have metadata with an rtype field.
         ;(format t "Inside the loop. ~% Name is ~a Scales-metadata mark is ~a~% scales is ~a~%" name (scales-metadata mark) scales)
         (setf scale-metadata (cdr (assoc name (scales-metadata mark) :test #'string=))
               dimension (if (cdr (assoc "dimension" scale-metadata :test #'string=))
                             (cdr (assoc "dimension" scale-metadata :test #'string=))
                             (cdr (assoc name scales :test #'string=)))
               axis-args (if options
                             (progn
                               ;(warn "How to handle **(options.get(name, {})")
                               (list scale-metadata));;;Plus options.get(name)
                             scale-metadata)
               axis (%fetch-axis fig dimension (cdr (assoc name scales :test #'string=)))
               )
         ;(format t "After first setf in loop: ~% scale-metadata is ~a~% dimension is ~a~% axis-args is ~a~%" scale-metadata dimension axis-args)
         ;(format t "At if axis. Axis is ~a~%" axis)
         (if axis
             (progn
               ;(format t "Inside the part where axis is t~%")
               ;;(%apply-properties axis (getf options name nil)) THIS NEEDS TO WORK
               (if (assoc name axes :test #'string=)
                   (push (cons name axis) axes)
                   (setf (cdr (assoc name axes :test #'string=)) (axes))))   
             (progn
               ;(format t "Inside the part where axis is NIL~%")
               (setf key (traitlets:traitlet-metadata (class-of mark) (intern (string-upcase name) "BQPLOT") :atype))
               (when key
                 ;(format t "Inside the part were key is t. Key is ~a~% axis-args is ~a~%" key axis-args)
                 ;(format t "(list (intern (car axis-args) KEYWORD) (cdr (car axis-args)) ~a~%" (list (intern (caar axis-args) "KEYWORD") (cdar axis-args)))
                 (setf axis-type (cdr (assoc key *axis-types* :test #'string=)))
                       axis (apply #'make-instance axis-type :scale (cdr (assoc name scales :test #'string=)) (list (intern (string-upcase (caar axis-args)) "KEYWORD") (cdar axis-args))) ;;;How to handle **Axis_args
                       fig-axes (append fig-axes (list axis)))
               ;(format t "Passed the when key part. Woohoo~%")
               (%update-fig-axis-registry fig dimension (cdr (assoc name scales :test #'string=)) axis))))
    ;(format t "Passed %update-fig-axis-registry")
    (setf (axes-figure fig) fig-axes)
    ;(print "Done with axes")
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

;;;In python, the lambda list is def _mark_type(mark_type, options={}, axes_options={}, **kwargs.
;;;I'm going to get rid of the option optionals and just have a kwargs containing all the information.


(defun %draw-mark (mark-type kwargs)
  (print "Inside draw-mark")
  ;(format t "Mark-type: ~a~%" mark-type)
  ;(format t "kwargs: ~a~%" kwargs)
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
    ;(warn "Process color maps")
    #|
    (when cmap
      (if (assoc "color" options :test #'string=)
          (setf (cdr (assoc "color" options :test #'string=)) (list (cons 
    |#

    ;;;This loop is mimicking Python's mark_type.class_trait_names(scaled=True):
    ;(print "Going into first loop")
    (loop for (name . symb)  in (list (cons "x" 'bqplot::x) (cons "y" 'bqplot::y) (cons "color" 'bqplot::color))
       do
         (let ((dimension (%get-attribute-dimension name (make-instance mark-type))))
           ;;;This cond is the entire body of the loop. It consists of 3 conditions, and then a final 'else':
           ;;;First, we check to see if name is not contained in kwargs.
           ;;;If it is not (it is likely that color will not be in kwargs, for instance), then we do nothing.
           ;;;If name is in kwargs, then we check to see if name is in scales (scales is the value of :scales in kwargs, or nil if not present
           ;;;If name is not in scales, then we check to see if dimension is not in %context['scales'].
           ;(format t "DIMENSION: ~a~%" dimension)
           (cond ((not (getf kwargs (intern (string-upcase name) "KEYWORD")))
                  ;(format t "Not getf kwargs name is true. name is ~a and kwargs is ~a~%" name kwargs)
                  (values))
                 ((getf scales (intern name "KEYWORD"))
                  ;(format t "getf scales name is true. name is ~a and scales is ~a~%" name scales)
                  (when update-context
                    (setf (cdr (assoc dimension (cdr (assoc "scales" %context :test #'string=)) :test #'string=)) (cdr (assoc name scales :test #'string=)))))
                 ((not (assoc dimension (cdr (assoc "scales" %context :test #'string=)) :test #'string=))
                  ;(format t "not assoc dimension cdr assoc \"scales\" %context is true. dimension is ~a and %context is ~a~%" dimension %context)
                  (let* ((r-type (traitlets:traitlet-metadata mark-type symb :rtype))
                         ;;(traitlet mark_type.class_traits[name]
                         ;;(dtype (validate the datatype is correct)
                         (compat-scale-types (loop for (str . class) in *scale-types* when (string= r-type (r-type (clos:class-prototype class))) collect class))
                         (sorted-scales (stable-sort compat-scale-types #'(lambda (x y) (< (precedence (clos:class-prototype x)) (precedence (clos:class-prototype y)))))))
                    (if (assoc name scales :test #'string=)
                        (setf (cdr (assoc name scales :test #'string=)) (apply #'make-instance (last sorted-scales)))
                        (push (cons name (apply #'make-instance (last sorted-scales))) scales))
                    ;(warn "Did I handle scales[name] = sorted_scales[-1](**options.get(name, {}))?")
                    ;(warn "You didn't, fool")
                    ;(warn "Hold on now, maybe you did")
                    ;(warn "Alright I ruined it")
                    (when update-context
                      (if (assoc dimension (cdr (assoc "scales" %context :test #'string=)) :test #'string=)
                          (setf (cdr (assoc dimension (cdr (assoc "scales" %context :test #'string=)) :test #'string=)) (cdr (assoc name scales :test #'string)))
                          (push (cons dimension (cdr (assoc name scales :test #'string=))) (cdr (assoc "scales" %context :test #'string=)))))))
                 (t
                  ;(format t "NOTHING IS TRUE!!! Dimension is ~a~%" dimension)
                  (if (assoc name scales :test #'string=)
                      (setf (cdr (assoc name scales :test #'string=)) (cdr (assoc dimension (cdr (assoc "scales" %context :test #'string=)) :test #'string=)))
                      (push (cons name (cdr (assoc dimension (cdr (assoc "scales" %context :test #'string=)) :test #'string=))) scales))))))
    ;(format t "We made it out of the loop!!!! ~% Scales is ~a and mark is ~a~%" scales mark)
    (setf kwargs (append kwargs (list :scales-mark scales)))
    ;(format t "Updated kwargs. It is now ~a~%" kwargs)
    (setf mark (apply #'make-instance mark-type (list :scales-mark scales)))
    ;(format t "After updating kwargs and mark.~% kwargs is now ~a and mark is ~a~%" kwargs mark)
    (if (assoc "last_mark" %context :test #'string=)
        (setf (cdr (assoc "last_mark" %context :test #'string=)) mark)
        (push (cons "last_mark" mark) %context))
    ;(format t "After if assoc last_mark %context~% %context is now ~a~%" %context)
    ;(format t "Marks fig is ~a~% and mark is ~a~%" (marks fig) mark)
    (setf (marks fig) (concatenate 'vector (marks fig) (list mark)))
    ;(format t "Calling axes with :mark ~a :options ~a~%" mark axes-options)
    (when (getf kwargs :axes t)
        (axes :mark mark :options axes-options))
    ;(print "Done with %draw-mark")
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
      (let ((key (cdr (assoc "current_key" %context :test #'string=))))
	(when key
	  (setf  (cdr (assoc key (cdr (assoc "scale_registry" %context :test #'string=)))) nil ))))))

;;needs to be checked 
(defun current-figure ()
  (print "Inside current-figure")
  (unless (cdr (assoc "figure" %context :test #'string=))
    (figure)) 
  (cdr (assoc "figure" %context :test #'string=)))


;(defun get-context ())

;(defun set-context (context))


(defun %fetch-axis (fig dimension scale)
  ;;(return-from %fetch-axis nil)
  (print "In %fetch-axis")
  ;(format t "inside %fetch-axis~% fig is ~a~% dimension is ~a~% scale is ~a~%" fig dimension scale)
  (let ((axis-registry (axis-registry fig)))
    ;(format t "axis-registry is ~a~%" axis-registry)
    (unless axis-registry
      (return-from %fetch-axis nil))
    (let*
        ((dimension-data (cdr (assoc dimension axis-registry :test #'string=)))
         (dimension-scales (cdr (assoc "scale" dimension-data :test #'string=)))
                             ;(loop for dim in dimension-data collect (cdr (assoc "scale" dim :test #'string=)))))
         (dimension-axes (cdr (assoc "axis" dimension-data :test #'string=))))
                                        ;(loop for dim in dimension-data collect (cdr (assoc "axis" dim :test #'string=)))))
      ;(format t "Out of let*. dimension data is ~a~% dimension-scales is ~a~% dimension axes is ~a~%" dimension-data dimension-scales dimension-axes)
      (if dimension-scales
          dimension-scales
          nil))))

(defun %update-fig-axis-registry (fig dimension scale axis)
  (print "In %update-fig-axis-registry")
  ;(format t "In %update-fig-axis-registry.~% fig is ~a~% dimension is ~a~% scale is ~a~% axis is ~a~%" fig dimension scale axis)
  (let* ((axis-registry (axis-registry fig))
	 (dimension-scales (cdr (assoc dimension axis-registry :test #'string=))))
    (setf dimension-scales (append dimension-scales (list (cons "scale" scale) (cons "axis" axis))))
    ;(format t "Entering first if statement. axis-registry is ~a~%" axis-registry)
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
  (format t "In %get-attribute-dimension~% trait-name is ~a~% mark-type is ~a~%" trait-name mark-type)
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


      
	  
      
