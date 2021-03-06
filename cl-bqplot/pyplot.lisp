(in-package :bqplot)

(defparameter %context (list (cons "figure" nil)
                             (cons "figure_registry" (make-hash-table :test 'equalp))
                             (cons "scales" nil)
                             (cons "scale_registry"  (make-hash-table :test 'equalp))
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

(defvar Keep (list "Keep" "bqplot.pyplot" "Used in bqplot.pyplot to specify that the same scale should be used for a certain dimension."))

;;;(defun hashtable (data v)
;;;(warn "How to try data[v]"))

(defun reset-context ()
    (setf %context (list (cons "figure" nil)
                             (cons "figure_registry" (make-hash-table :test 'equalp))
                             (cons "scales" nil)
                             (cons "scale_registry" (make-hash-table :test 'equalp))
                             (cons "last_mark" nil)
                             (cons "current_key" nil))))

(defun reset-context-scales ()
  (setf %context (list (cons "figure" ([] %context "figure"))
                       (cons "figure_registry" ([] %context "figure_registry"))
                       (cons "scales" nil)
                       (cons "scale_registry" ([] %context "scale_registry"))
                       (cons "last_mark" ([] %context "last_mark"))
                       (cons "current_key" ([] %context "current_key")))))

(defun show (&key (key nil) (display-toolbar t))
  (let ((figure nil))
    (if key
	(setf figure (gethash key ([] %context "figure_registry")))
	(setf figure ([] (current-figure) "1")))
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
                                        ;(print "About to enter first let")
;;;Now begins the translation of python code.
  (let ((scales-arg (getf kwargs :scales)))
    (remf kwargs :scales)
                                        ;(print "checking for current_key in context")
    (setf ([] %context "current_key") key)
                                        ;(logg 3 "Checking fig... ~% FIG: ~a~%" fig)
    (if fig
	(progn
	  (setf ([] %context "figure") (list fig))
	  (when key
	    (setf (gethash key ([] %context "figure_registry")) fig))
	  (loop for arg in kwargs do
	    (unless ([]-contains ([] %context "figure") arg)
	      (setf ([] %context "figure") (append ([] %context "figure") (cons arg ([] kwargs arg)))))))
	(progn
                                        ;(print "In progn sequence where fig is NIL")
                                        ;(logg 3 "KEY: ~a ~%" key)
	  (if (not key)
              (progn
                                        ;(print "in progn where key is NIL")
                (setf ([] %context "figure") (make-instance 'figure)))
	      (progn
                                        ;(print "In progn where key is not nil")
                (when (null (gethash key ([] %context "figure_registry")))
                  (unless (getf kwargs :title)
                    (push (format nil "Figure ~a" key) kwargs)
                    (push :title kwargs))
                  (setf (gethash key ([] %context "figure_registry")) (apply #'make-instance 'figure kwargs)))
                (setf ([] %context "figure") (gethash key ([] %context "figure_registry")))
                                        ;(warn "How to Add a slot for each argument in kwargs")
                (scales :key key :scales scales-arg)
                (loop with instance = ([] %context "figure")
                      with class = (class-of instance)
                      for (key value) on kwargs by #'cddr
                      for class-slots = (clos:class-slots class)
                      for effective-slot-definition = (find-if (lambda (slot) (member :title (clos:slot-definition-initargs slot))) class-slots)
                      for slot-index = (clos:slot-definition-location effective-slot-definition)
                      do (setf (clos:standard-instance-access instance slot-index) value))
                ))))
    ;;(print "After fig IF statement, about to return ([] %context \"figure\"")
    (when (eq (axis-registry ([] %context "figure")) nil)
      (setf (axis-registry ([] %context "figure")) nil))
    ([] %context "figure")))
        
(defun close (key)
  (let ((figure-registry ([] %context "figure_registry"))
        (fig nil))
    (unless (gethash key figure-registry)
      (return-from close))
    (when (eq ([] %context "figure") (gethash key figure-registry)) (make-instance 'figure))
    (setf fig (gethash key figure-registry))
    ;;if hasattr(fig, 'pyplot')
    ;;fig.pyplot.close()
    (remhash key ([] %context "figure_registry"))
    ;;del figure_registry[key]
    (remhash key ([] %context "scale_registry"))
    ;;del _context['scale_registry'][key]
    (values)))

;;;(defun %process-data (&rest kwarg-names &key &allow-other-keys)
  ;;;(warn "TODO: Make %process data"))

;TODO

(defun scales (&key (key nil) (scales nil))
  "Creates and switches between context scales.

    If no key is provided, a new blank context is created.

    If a key is provided for which a context already exists, the existing
    context is set as the current context.

    If a key is provided and no corresponding context exists, a new context is
    created for that key and set as the current context.

    Parameters
    ----------
    key: hashable, optional
        Any variable that can be used as a key for a dictionary
    scales: dictionary
        Dictionary of scales to be used in the new context

    Example
    -------

        >>> scales(scales={
        >>>    'x': Keep,
        >>>    'color': ColorScale(min=0, max=1)
        >>> })

    This creates a new scales context, where the 'x' scale is kept from the
    previous context, the 'color' scale is an instance of ColorScale
    provided by the user. Other scales, potentially needed such as the 'y'
    scale in the case of a line chart will be created on the fly when
    needed.

    Notes
    -----
    Every call to the function figure triggers a call to scales.

    The `scales` parameter is ignored if the `key` argument is not Keep and
    context scales already exist for that key.
    "
  (let ((old-ctxt ([] %context "scales")))
    (if (null key)
        ;;No key is provided
        (setf ([] %context "scales")
              (loop for (k . scales-k) in scales
                    collect (if (not (eq scales-k Keep))
                                (cons (%get-attribute-dimension k) scales-k)
                                ([] old-ctxt (%get-attribute-dimension k)))))
        ;;A key is provided
        (progn
          (unless (gethash key ([] %context "scale_registry"))
            (setf (gethash key ([] %context "scale_registry"))
                  (loop for (k . scales-k) in scales
                        collect (if (not (eq ([] scales k) keep))
                                    (cons (%get-attribute-dimension k) scales-k)
                                    ([] old-ctxt (%get-attribute-dimension k))))))
          (setf ([] %context "scales") (gethash key ([] %context "scale_registry")))))))

;TODO

(defun xlim (low high)
  (set-lim low high "x"))

(defun ylim (low high)
  (set-lim low high "y"))

(defun set-lim (low high name)
  (let ((scale (assoc (%get-attribute-dimension name) ([] %context "scales"))))
    (setf (min scale) low
	  (max scale) high)
  scale))

(defun axes (&rest kwargs &key (mark nil) (options nil) &allow-other-keys)
;;;Remove mark and options from kwargs
  (remf kwargs :mark)
  (remf kwargs :options)
  (unless mark
    (let ((new_mark ([] %context "last_mark")))
      (if new_mark
          (setf mark ([] %context "last_mark"))
          (return-from axes nil))))
  (let* ((fig (getf kwargs :figure (current-figure)))
         (scales (scales-mark mark))
         (_ (logg 3 "About to enter loop scales is ~s~%" scales))
         (_ (logg 3 "About to fig-axes  (axes-figure fig) -> ~s~%" (axes-figure fig)))
         (fig-axes (loop for axes-instance across (axes-figure fig) collect axes-instance))
         (_ (logg 3 "fig-axes -> ~s~%" fig-axes))
         (axes nil)
         (scale-metadata nil)
         (dimension nil)
         (axis-args nil)
         (axis nil)
         (key nil)
         (axis-type nil))
    (loop for (name . instance) in scales
          do
             ;;missing the function that checks to see if the scale is even needed
             ;; (if name not in mark.class_trait_names(scaled=True):)
             ;;mark.class_trait_names(scaled=True) returns a list of all slots
             ;; that have metadata with an rtype field.
             ;;(logg 3 "Inside the loop. ~% Name is ~a Scales-metadata mark is ~a~% scales is ~a~%" name (scales-metadata mark) scales)
             (setf scale-metadata ([] (scales-metadata mark) name)
                   dimension (if ([] scale-metadata "dimension")
                                 ([] scale-metadata "dimension")
                                 ([] scales name))
                   axis-args (if options
                                 (progn
                                        ;(warn "How to handle **(options.get(name, {})")
                                   (list scale-metadata)) ;;;Plus options.get(name)
                                 scale-metadata)
                   axis (%fetch-axis fig dimension ([] scales name)));;;CURRENT CODE DIES HERE
             (logg 3 "After first setf in loop: ~% scale-metadata is ~s~% dimension is ~s~% axis-args is ~s~%" scale-metadata dimension axis-args)
             (logg 3 "At if axis. Axis is ~s~%" axis)
             (if axis
                 (progn
                   ;;(logg 3 "Inside the part where axis is t~%")
                   ;;(%apply-properties axis (getf options name nil)) THIS NEEDS TO WORK
                   (if (assoc name axes :test #'string=)
                       (push (cons name axis) axes)
                       (setf ([] axes name) axes)))   
                 (progn
                   (setf key (traitlets:traitlet-metadata (class-of mark)
                                                          (intern (string-upcase name) "BQPLOT") :atype))
                   (when key
                     (setf axis-type ([] *axis-types* key)
                           axis (apply #'make-instance axis-type :scale ([] scales name) (list (intern (string-upcase (caar axis-args)) "KEYWORD") (cdar axis-args))) ;;;How to handle **Axis_args
                           fig-axes (concatenate 'vector fig-axes (vector axis)))
                     (%update-fig-axis-registry fig dimension ([] scales name) axis)))))
    (setf (axes-figure fig) fig-axes)
    (logg 3 "end of axes axes -> ~s~%" axes)
    axes))


(defun %set-label (label mark dim &rest kwargs &key &allow-other-keys)
  (unless mark
    (setf mark ([] %context "last_mark")))
  (unless mark
    (return-from %set-label nil))
  (let* ((fig (getf kwargs :figure))
	(scales (scales mark))
	(scales-metadata (getf (scales-metadata mark) dim))
	 (scale (getf scales dim)))
    (unless scale
      (return-from %set-label nil))
    (let* ((dimension (getf scales-metadata "dimension"))
	  (axis (%fetch-axis fig dimension ([] scales dim))))
      (unless dimension  
	(setf dimension ([] scales dim)))
      ;(when axis
                                        ;(%apply-properties axis (cons "label" label)))))
      ))
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
  (loop for a in (axes-figure fig)
     do
       (setf (grid-lines a) value)))

(defun title (label &key (style nil) &allow-other-keys) ;no need for kwargs but apparently we're not allowed to say &rest &key
  (let ((fig (current-figure)))
    (setf (title-figure fig) label)
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
     (setf ([] scales "y") (scale-y fig))
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




(defun class-scales-metadata (class)
  "This gets the scales-metdata slot value for the class (it must be :allocation :class and readonly).
We use the very, very low level standard-instance-access function so that we don't go through slot-value-using-class
because that method uses a mutex."
  (clos:standard-instance-access
   (clos:class-prototype class)
   (clos:slot-definition-location
    (find 'bqplot::scales-metadata (clos:class-slots class) :key #'clos:slot-definition-name))))

;;;In python, the lambda list is def _mark_type(mark_type, options={}, axes_options={}, **kwargs.
;;;I'm going to get rid of the option optionals and just have a kwargs containing all the information.


(defun %draw-mark (mark-type kwargs)
                                        ;(logg 3 "Mark-type: ~a~%" mark-type)
                                        ;(logg 3 "kwargs: ~a~%" kwargs)
  (let ((mark-class (if (symbolp mark-type)
                        (find-class mark-type)
                        mark-type))
        (fig (let ((fig (getf kwargs :figure (current-figure))))
               fig))
        (scales (getf kwargs :scales))
        (update-context (getf kwargs :update-context t))
        (cmap (getf kwargs :cmap))
        (options (getf kwargs :options))
        (axes-options (getf kwargs :axes-options))
        (mark nil)
        (temp-kwargs nil))
    (remf kwargs :fig)
    (remf kwargs :scales)
    (remf kwargs :cmap)
                                        ;(warn "Process color maps")
    #|
    (when cmap                          ; ;
    (if (assoc "color" options :test #'string=) ; ;
    (setf ([] options "color") (list (cons ; ;
    |#
;;;This loop is mimicking Python's mark_type.class_trait_names(scaled=True):
    (let ((dict (class-scales-metadata mark-class)))
      (loop for entry in dict
            for name = (car entry)
            do
               (let ((dimension (%get-attribute-dimension name mark-class)))
;;;This cond is the entire body of the loop. It consists of 3 conditions, and then a final 'else':
;;;First, we check to see if name is not contained in kwargs.
;;;If it is not (it is likely that color will not be in kwargs, for instance), then we do nothing.
;;;If name is in kwargs, then we check to see if name is in scales (scales is the value of :scales in kwargs, or nil if not present
;;;If name is not in scales, then we check to see if dimension is not in %context['scales'].
                 (cond ((not (getf kwargs (intern (string-upcase name) "KEYWORD")))
                        (values))
                       ((getf scales (intern name "KEYWORD"))
                        (when update-context
                          (setf ([] ([] %context "scales") dimension) ([] scales name))))
                       ((not ([]-contains ([] %context "scales" ) dimension))
                        (let* ((r-type (traitlets:traitlet-metadata mark-class name :rtype))
                               ;;(traitlet mark_type.class_traits[name]
                               ;;(dtype (validate the datatype is correct)
                               (compat-scale-types
                                 (loop for (scale-sym . class) in *scale-types*
                                       when (string-equal r-type (r-type (clos:class-prototype class)))
                                         collect class))
                               (sorted-scales (progn
                                                (stable-sort
                                                 compat-scale-types
                                                 #'(lambda (x y) (< (precedence (clos:class-prototype x))
                                                                    (precedence (clos:class-prototype y))))))))
                          (setf ([] scales name)
                                (let ((sorted-scales-1 (car (last sorted-scales)))
                                      (opts ([] options name nil)))
                                  (apply #'make-instance sorted-scales-1 opts))))
                                        ;(warn "Did I handle scales[name] = sorted_scales[-1](**options.get(name, {}))?")
                                        ;(warn "You didn't, fool")
                                        ;(warn "Hold on now, maybe you did")
                                        ;(warn "Alright I ruined it")
                        (when update-context
                          (setf ([] ([] %context "scales" ) dimension) ([] scales name))))
                       (t
                                        ;(logg 3 "NOTHING IS TRUE!!! Dimension is ~a~%" dimension)
                        (setf ([] scales name) ([] ([] %context "scales") dimension)))))))
                                        ;(logg 3 "We made it out of the loop!!!! ~% Scales is ~a and mark is ~a~%" scales mark)
    (setf temp-kwargs (append kwargs (list :scales-mark scales)))
                                        ;(logg 3 "Updated kwargs. It is now ~a~%" kwargs)
    (setf mark (apply #'make-instance mark-class temp-kwargs))
                                        ;(logg 3 "After updating kwargs and mark.~% kwargs is now ~a and mark is ~a~%" kwargs mark)
    (setf ([] %context "last_mark") mark)
                                        ;(logg 3 "After if assoc last_mark %context~% %context is now ~a~%" %context)
                                        ;(logg 3 "Marks fig is ~a~% and mark is ~a~%" (marks fig) mark)
    ;; This next statement should cause the marks for the figure to be updated
    (setf (marks fig) (concatenate 'vector (marks fig) (list mark)))
                                        ;(logg 3 "Calling axes with :mark ~a :options ~a~%" mark axes-options)
    (when (getf kwargs :axes t)
      (axes :mark mark :options axes-options))
    ;;(print "Done with %draw-mark")
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
    ;(unless x
    ; (setf x (%infer-x-for-line y)))
    ;(setf kwargs (append kwargs (list :x x :y y)))
    (if  marker-str
         (progn
           (strip marker-str)
           (multiple-value-bind (line-style color marker) (%get-line-styles marker-str)
             (if (and marker (not line-style))
                 (progn
                   (when color
                     (setf kwargs (append kwargs (list :default-colors (list color)))))
                   (return-from plot (%draw-mark 'scatter kwargs)))
                 (progn
                   (if line-style
                       (setf kwargs (append kwargs (list :line-style line-style)))
                       (setf kwargs (append kwargs (list :line-style "solid"))))
                   (when marker
                     (setf kwargs (append kwargs (list :marker marker))))
                   (when color
                     (setf kwargs (append kwargs (list :color (list color)))))
                   (return-from plot (%draw-mark 'lines kwargs))))))
         (%draw-mark 'lines kwargs))))

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
    (setf ([] kwargs "image") ipyimage)))
					;^^^^INCOMPLETE^^^^^^
;;(defun OHLC (args);; kwarg and args 
;;(when (= (len args) 2)
;;(setf kwargs (append kwargs (cdr (assoc 0 args :test #'equalp))))
;;kwargs (append kwargs (list :y (cdr (assoc 1 args :test #'equalp))))))
;;(when (= (len args) 1)
    ;(setf kwargs (append kwargs (list :y (cdr (assoc 0 args :test #'equalp))))
          ;length (len (cdr (assoc 0 args :test #'equalp=))) 
	  ;kwargs (appends kwargs (list :x (array length))))) ;;need to change arange 
  ;(%draw-mark (OHLC kwargs)))

;;;OHCL needs to be completely recoded to take into account kwargs

(defun scatter (x y &rest kwargs &key &allow-other-keys)
  (let ((new-kwargs (list* :x x :y y kwargs)))
    (%draw-mark 'scatter new-kwargs)
    ))

(defun hist (sample &rest kwargs &key (options nil)  &allow-other-key)
 (remf kwargs :option)
 (setf kwargs (append kwargs (list :sample sample)))
 (let ((scales (getf kwargs ':scales))(dimension))
   (remf kwargs ':scales)
   (unless (member "count" scales)
     (setf dimension (%get-attribute-dimension "count" (find-class 'Hist)))
     (if (member dimension ([] %context "scales"))
	 (setf scales (append scales (list :count (nth dimension ([] %context "scales")))))
	 (progn (setf ([] scales "count") (make-instance 'linear-scale (getf options "count")))
		(setf (nth dimension ([] %context "scales")) ([] scales "count")))))
   (setf ([] kwargs "scales") scales))
 (%draw-mark 'Hist :options options kwargs))

(defun bin (sample &rest kwargs &key (options nil) &allow-other-keys)
 (let ((scales)(dimension))
   (setf kwargs (append kwargs (list :sample sample))
         scales ([] kwargs "scales"))
   (remf kwargs :scales)
   (loop for xy in (list "x" "y") do
     (unless (member xy scales)
       (progn (setf dimension (%get-attribute-dimension xy (find-class 'bars)))
	      (if (member dimension ([] %context "scales"))
		  (setf ([] scales xy) (nth dimension ([] %context "scales")))
		  (progn (setf ([] scales xy) (make-instance 'linear-scale (getf options xy)))
			 (setf (nth dimension ([] %context "scales")) ([] scales xy)))))))
   (setf kwargs (append kwargs (list :scales scales)))
   (%draw-mark 'bins :options options kwargs)))


;;checked
(defun bar (x y &rest kwargs &key &allow-other-key)
  (setf kwargs (append kwargs (list :x x))
	kwargs (append kwargs (list :y y)))
  (%draw-mark 'bars kwargs))

;;need to check the class boxplot 
(defun boxplot (x y &rest kwargs &key &allow-other-key)
  (setf kwargs (append kwargs (list :x x))
	kwargs (append kwargs (list :y y)))
  (%draw-mark 'boxplot kwargs))

#|
(defun barh (arg kwargs) ;args and kwargs 
  (setf kwargs (append kwargs (list :orientation "horizontal")))
  (bar (args kwargs)));;arg kwargs 
|#

;;checked 
(defun pie (sizes &rest kwargs &key &allow-other-key)
   (setf kwargs (append kwargs (list :sizes sizes)))
  (%draw-mark 'pie  kwargs))
  
(defun label (text &rest kwargs &key &allow-other-key)
  (setf kwargs (append kwargs (list :text text)))
  (%draw-mark 'label kwargs))
  
(defun geo (map-data &rest kwargs &key &allow-other-key)
  (let ((scales (getf kwargs :scales ([] %context "scales")))
	(options (getf kwargs :options)))
    (unless (member "projections" scales)
      (setf ([] scales "projection") (make-instance 'mercator (getf options "projection"))))
    (setf ([] kwargs "scales") scales)
    ;(if (isinstance map-data string-types)
	;(setf (cdr (assoc "map-data" kwargs :test #'string=)) (topo-load ));figure out how the string works
	;(setf (cdr (assoc "map-data" kwargs :test #'string=))(map-data)))
    )
    (%draw-mark 'map kwargs))

;;checked 
(defun heat-map (color &rest kwargs &key &allow-other-key)
  (setf kwargs (append kwargs (list :color color)))
  (%draw-mark 'heat-map kwargs))

;;checked
(defun grid-heat-map (color &rest kwargs &key &allow-other-key)
    (setf kwargs (append kwargs (list :color color)))
   (%draw-mark 'grid-heat-map kwargs))

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
  (nth dimension ([] %context "scales")))
#||
(defun %create-selector (int-type func trait &rest kwargs &key &allow-other-keys)
  (let ((interaction (%add-interaction int-type kwargs)))
    ;(when func
      ;(on-trait-change interaction func trait))
    interaction))
|#

#||
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
|#
(defun clear-figure ()
  (let ((fig ([] %context "figure")))
    (when fig 
      (setf (marks fig) nil
	    (axes-figure fig) nil
            (axis-registry fig) nil
            ;;("axis-registry" fig) nil) ;; did i handle setattr right?
	    ([] %context "scales") nil)
      (let ((key ([] %context "current_key")))
	(when key
          (remhash key ([] %context "scale_registry")) nil )))))

;;needs to be checked
#|| TODO: DOESNT WORK
(defun current-figure ()
  (unless ([]-contains %context "figure")
    (figure)) 
  ([] %context "figure"))
|#

(defun current-figure ()
  (unless (cdr (assoc "figure" %context :test #'string=))
    (figure))
  ([] %context "figure"))

;(defun get-context ())

;(defun set-context (context))


(defun %fetch-axis (fig dimension scale)
  ;;(return-from %fetch-axis nil)
  (logg 3 "inside %fetch-axis~% fig is ~s~% dimension is ~s~% scale is ~s~%" fig dimension scale)
  (let ((axis-registry (axis-registry fig)))
    (logg 3 "axis-registry is ~s~%" axis-registry)
    (let* ((dimension-data ([] axis-registry dimension nil))
           (_ (logg 3 "dimension-data -> ~s~%" dimension-data))
           (dimension-scales (loop for (dim . value) across dimension-data
                                when (string= (car dim) "scale")
                                  collect (cdr dim)))
                                ;do (logg 3 "dim -> ~s  value -> ~s~%" dim value)
                                  ;;;TRYING SOMETHING NEW. In the above code, I'm assuming that dim will always be a cons cell instead of an alist like we had previously thought.  
                                   ;collect ([] dim "scale")));CURRENT CODE DIES HERE
           (_ (logg 3 "dimension-scales -> ~s~%" dimension-scales))
           (dimension-axes (loop for (dim . value) across dimension-data
                              when (string= (car dim) "axis")
                                collect (cdr dim)))
           (dimension-scales-index (position (cons "scale" scale) dimension-scales :test #'equal)))
      (if dimension-scales-index
          (cdr (elt dimension-axes dimension-scales-index))
          nil))))

(defun %update-fig-axis-registry (fig dimension scale axis)
  (logg 3 "In %update-fig-axis-registry.~% fig is ~a~% dimension is ~a~% scale is ~a~% axis is ~a~%" fig dimension scale axis)
  (logg 3 "axis-registry is ~s~%" (axis-registry fig))
  (let* ((axis-registry (axis-registry fig))
         (dimension-scales ([] axis-registry dimension nil)))
    (setf dimension-scales (concatenate 'vector
                                        dimension-scales
                                        (vector (list (cons "scale" scale)
                                                      (cons "axis" axis)))))
    (setf ([] axis-registry dimension) dimension-scales)
    (setf (axis-registry fig) axis-registry)
    (logg 3 "final axis-registry is ~s~%" axis-registry)
    ))
  
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

(defun %get-attribute-dimension (trait-name &optional mark-class)
  (unless mark-class
    (return-from %get-attribute-dimension trait-name))
  (let ((scale-metadata (class-scales-metadata mark-class)))
    ([] ([] scale-metadata trait-name) "dimension")))



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
#| TO-DO: Figure out better %get-line-styles
(defun %get-line-styles (marker-str)
  (defun %extract-marker-value (marker-str code-dict)
    (let (val)
      (loop for code in code-dict do
	   (when (member code marker-str)
	     (setf val ([] code-dict code))
					;something should break the loop right here
		 ))))
  (loop for code-dict in (list line-style-codes color-codes marker-codes) do
       (%extract-marker-value marker-str code-dict)
       ))

  |#

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


      
	  
      
