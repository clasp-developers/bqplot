(in-package :bqplot)

(defparameter %context (list (cons "figure" nil)
			     (cons "figure_registry" nil)
			     (cons "scales" nil)
			     (Cons "Scale_registry" nil)
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
        (prgn 
	 (unless (pyplot figure)
	   (setf (pyplot figure) (make-instance 'toolbar :figure figure)))
	 (display (make-instance 'vbox :children (vector figure (pyplot figure)))))
        (display figure)))
  (values))

(defun figure (&rest kwargs &key (key nil) (fig nil) &allow-other-keys)
  ;;;We don't want key and fig to actually be in the kwargs plist
  (remove key kwargs)
  (remove :key kwargs)
  (remove :fig kwargs)
  (remove fig kwargs)
  ;;;Now begins the translation of python code.pa
  (let ((scales-arg (getf kwargs ':scales)))
    ;;Make getf an effective pop of the (:scales value)
    (remove ':scales kwargs)
    (remove scales-arg kwargs)
    (setf (cdr (assoc "current_key" %context :test #'string=)) key)
    (if fig
	(progn
	  (setf (cdr (assoc "figure" %context :test #'string=)) fig)
	  (when key
	    (setf (nth key (cdr (assoc "figure_registry" %context :test #'string=))) fig)))
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
                (warn "How to Add a slot for each argument in kwargs")))))
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

(defun scales (&key (key nil) (scales nil))
  (warn "TODO: Make scales"))

(defun xlim (low high)
  (set-lim low high "x"))

(defun ylim (low high)
  (set-lim low high "y"))

(defun set-lim (low high name)
  (let (scale (cdr (assoc (%get-attribute-dimension name) (cdr (assoc "scales" %context :test #'string=)))))
    (setf (min scale) low
          (max scale) high)
  scale))

#|(defun axes (&rest kwargs &key (mark nil) (options nil) &allow-other-keys)
  ;;;Remove mark and options from kwargs
  (setf kwargs (remove mark kwargs)
        kwargs (remove ':mark kwargs)
        kwargs (remove options kwargs)
        kwargs (remove ':options kwargs))
  (unless mark
    (let ((new-mark (cdr (assoc "last-mark" %context :test #'string=)))
      (if new-mark
          (setf Mark (cdr (assoc "last-mark" %context :test #'string=)))
          (return-from axes nil)))))
  (let ((fig (cdr (assoc "figure" current-figure :test #'string=)))
    (unless fig
      (setf fig (current-figure)))
    (let ((scales (scales mark))
          (fig-axes (loop for axis in (axes fig) collect axis))
          (axes nil))
      (loop for name in scales
            do
               (unless (member name ((getf scales-metadata name nil) mark)
                               
      )))))))|#
;;;FINISH AXES
#|
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
|#
#|
(defun %set-label (label mark dim &rest kwargs &key &allow-other-keys)
  (unless mark
    (setf mark (cdr (assoc "last-mark" %context :test #'string=))))
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
|#

(defun xlabel (&rest kwargs &key (label nil) (mark nil) &allow-other-keys)
  (remf kwargs :label)
  (remf kwargs :mark)
  (%set-label (label mark "x" kwargs)))

(defun ylabel (&rest kwargs &key (label nil) (mark nil) &allow-other-keys)
  (remf kwargs :label)
  (remf kwargs :mark)
  (%set-label (label mark "y" kwargs)))


;;;for plot func pass in x and y as the parameres instead of the arg and kwarg 


(defun grids (&key (fig nil) (value "solid"))
  (unless fig
    (setf fig (current-figure)))
  (loop for a in (axes fig)
     do
       (setf (grid-lines a) value)))

(defun title (label &key (style nil))
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
  (unless (getf kwargs ':stroke-width)
    (setf kwargs (append kwargs (cons ':stroke-width 1))))
  (let* ((scales (getf kwargs ':scales))
        (fig (getf kwargs ':figure))
        (x nil)
        (y nil)
        (scales-x (assoc "x" scales :test #'string=)))
    (unless fig
      (setf fig (current-figure)))
    (if scales-x
        (setf (cdr scales-x) (Scale-X fig))
        (setf scales (append scales (cons "x" (scale-x fig)))))
    (remf kwargs :scales)
    (setf level (vector level))
    (if (= (length (shape level)) 0)
        (setf x (list 0 1)
              y (list level level))
        (setf x (list 0 1)
              y (column-stack (list level level))))
  (plot x y :scales scales :preserve-domain (list (cons "x" t)(cons "y" (getf kwargs ':preserve-domain))) :axes nil :update-context nil kwargs)))

;;NEED TO ADD 
;;v-line
;;process-cmap

(defun set-cmap (cmap)
  (let ((scale (cdr (assoc "color" (cdr (assoc "scales" %context :test #'string=)) :test #'string=))))
    (loop for (k . v) in (%process-cmap cmap)  
       do
	 (setf (k scale) v) ;; is this right
	 )
    scale))
#|
(defun %draw-mark (mark-type &rest kwargs &key (options nil) (axes-options nil) &allow-other-keys)
  (remf kwargs :options)
  (remf kwargs :axes-options)
  (let ((fig (getf kwargs :figure))
    (scales (getf kwargs :scales))
    (update-content (getf kwargs :update-content))
    (cmap (getf kwargs :cmap)))
  (remf kwargs :figure)
  (remf	kwargs :scales)
  (remf kwargs :update-content)
  (remf kwargs :cmap)
  (unless fig
    (setf fig (current-figure)))
  (unless update-context (setf update-content t))
  (when cmap
    (if (assoc "color" options :test #'string=)
	(values)
      ;;;(setf (cdr (assoc "color" options :test #'string=)) dict(options.get('color', {}, **_process_cmap(cmap))))
	(append (cons "color" nil) options)));;;FIXME
;;;finish draw mark process c map 
|#

  ;;;checked gth
(defun %infer-x-for-line (y)
  (let (array-shape (shape y))
    (when ( = (length array-shape) 0 )
      (return-from %infer-x-for-line nil))
    (when ( = (length array-shape) 1 )
      (return-from %infer-x-for-line (loop for i upto (1- (first array-shape)) collect i))) 
    (when ( > (length array-shape) 1)
      (return-from %infer-x-for-line (loop for i upto (1- (second array-shape)) collect i)))))

#|(defun plot (x y marker-str &rest kwargs &key &allow-other-keys) ;;;need to fix this. What to put in just args and parse the args from keyword args 
  (let (( marker-str nil))
    (cond ((= (length args) 1)
	   (setf kwargs (appends kwargs (list :y (cdr (assoc 0 args :test #'equalp=)))))
	   (if (getf kwargs :index-data)
	       (setf kwargs (appends kwargs (list :x (cdr (assoc "index-data" args :test #'string=)))))
	       (setf kwargs (appends kwargs (list :x (%infer-x-for-line (cdr (assoc 0 args :test #'equalp=))))))))
	  (( = (length args) 2 )
	   (if (getf kwargs ;;how to check if the passed in arg[0] is a string
		     (progn 
		       (setf (kwargs (append kwargs (list :y (cdr (assoc 0 args :test #'equalp=)))))
			     (kwargs (append kwargs (list :x (%infer-x-for-line (cdr (assoc 0 args :test #'equalp=))))))
			     (marker-str ((strip) ((cdr (assoc 1 args :test #'equalp=)))))))
		     (progn
		       ((setf (kwargs (append kwargs (list :x (cdr (assoc 0 args :test #'equalp=)))))
			      (kwargs (append kwargs (list :y (cdr (assoc 1 args :test #'equalp=)))))))))))
	)))|#
			   
		        
				     

#|(defun imshow (image format kwargs &key &allow-other-keys)
  (let (ipyimage)
  (cond ((= format "widget")
	 (setf ipyimage image))
	((= format "filename")
	 
	 )
	(t
	 (setf ipyimage ;;need help kevin 
	   ))))
  
	
	|#
      
  ;;need help
  



;(defun OHLC ();; kwarg and args 
  ;(when (= (length args) 2)
    ;(setf kwargs (append kwargs (list :x (cdr (assoc 0 args :test #'equalp))))
	  ;kwargs (append kwargs (list :y (cdr (assoc 1 args :test #'equalp))))))
  ;(when (= (length args) 1)
    ;(setf kwargs (append kwargs (list :y (cdr (assoc 0 args :test #'equalp))))
          ;length (length (cdr (assoc 0 args :test #'equalp=))) 
	  ;kwargs (appends kwargs (list :x (arange length))))) ;;need to change arange 
  ;(%draw-mark (OHLC kwargs)))
   

;;uses a plist so you need to apend. Assoc used for alist and cons 
;;checked
(defun scatter (x y &rest kwargs &key &allow-other-keys)
  (setf kwargs (append kwargs (list :x x))
	kwargs (append kwargs (list :y y)))
  (%draw-mark (find-class 'scatter) kwargs))

#|(defun hist (sample &rest kwargs &key (option nil)  &allow-other-key)
 (remf kwargs :option)
 (setf kwargs (append kwargs (list :sample sample)))
 (let ((scales (getf kwargs ':scales))(dimension))
   (remf kwargs ':scales)
   (unless (member "count" scales)
     (setf dimension (%get-attribute-dimension ("count" (find-class 'Hist)))
           (if (member dimension (cdr (assoc "scales" %context :test #'string=)))
               (setf scales (append scales (list :count (nth dimension (cdr (assoc "scales" %context :test #'string=))))))
          (progn (setf (cdr (assoc "count" scales :test #'string=)) (linear-scale (getf options "count")))
             (setf (nth dimension (cdr (assoc "scales" %context :test #'string=))) (cdr (assoc "count" scales :test #'string=)))))))
          (setf (cdr (assoc "scales" kwargs :test #'string=)) scales)
          (%draw-mark (find-class 'Hist) :options options kwargs)))|#

(defun bin (sample &rest kwargs &key (options nil) &allow-other-keys)
 (let ((scales)(dimension))
   (setf kwargs (append kwargs (list (:sample sample)))
      scales (cdr (assoc "scales" kwargs :test #'string=)))
   (remf kwargs :scales)
   (loop for xy in (list "x" "y")
      do
        (unless (member xy scales)
          (setf dimension (%get-attribute-dimension xy (find-class 'bars)))
          (if (member dimension (cdr (assoc "scales" %context :test #'string=)))
              (setf (cdr (assoc xy scales :test #'string=)) (nth dimension (cdr (assoc "scales" %context :test #'string=))))
              (setf (cdr (assoc xy scales :test #'string=)) (linear-scale (getf options xy))
                    (nth dimension (cdr (assoc "scales" %context :test #'string=))) (cdr (assoc xy scales :test #'string=))))))
   (setf kwargs (append kwargs (list (:scales scales))))
   (%draw-mark (find-class 'bins) :options options kwargs)))

#|(defun bin (sample (options nil) &rest kwargs &key &allow-other-key)
  (setf kwargs (appends kwargs (list :sample sample)))
  (let ((scales (getf kwargs :scales)))
  (remf kwargs :scales)))|#
;;finish this

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
  (%draw-mark (find-class pie)  kwargs))

;;checked 
(defun label (text &rest kwargs &key &allow-other-key)
  (setf kwargs (append kwargs (list :text text)))
  (%draw-mark (find-class 'label) kwargs))
  
(defun geo (map-data &rest kwargs &key &allow-other-key)
  (let ((scales  (getf :scales kwargs (cdr (assoc "scales" %context :test #'string=))))
	  (options (getf :options kwargs nil))))
  (unless (assoc "projection" scales :test #'string=)
    (warn "How do we **options")
 ;;;(setf scales (append scales (cons "projection" (make-instance 'mercator )
    )
  ;;kevin
  )

;;checked 
(defun heat-map (color &rest kwargs &key &allow-other-key)
  (setf kwargs (append kwargs (list :color color)))
  (%draw-mark (find-class 'heat-map)  kwargs))

;;checked
(defun grid-heat-map (color &rest kwargs &key &allow-other-key)
    (setf kwargs (append kwargs (list :color color)))
   (%draw-mark (find-class 'grid-heat-map) kwargs))

(defun %add-interaction (int-type &rest kwargs &key &allow-other-key)
  (let ((fig (getf kwargs :fig (current-figure)))
        (marks (getf :marks kwargs))) ;;need help with context last mark 
    (remf kwargs :figure)
    (remf kwargs :marks)
  ;;ned help with for loop
    (setf (getf kwargs :marks) marks)))
  ;;don know the rest 
  

;;checked 
(defun %get-context-scale (dimension)
  (nth dimension (cdr (assoc "scales" %context :test #'string=))))

  ;(cdr (assoc dimension (assoc "scales" %context :test #'string=)))

  ;; (setf (nth key (cdr (assoc "figure_registry" %context :test #'string=))) fig)))
;;; _context['figure_registry'][key] = fig
  ;;checked
  
(defun %create-selector (int-type func trait &rest kwargs &key &allow-other-key)
  (let ((interaction (%add-interaction (int-type kwargs))))
  (when func
    (on-trait-change interaction func trait))
  interaction))

;;need the seven below checked 
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

(defun clear ()
  (let ((fig (cdr (assoc "figure" %context :test #'string=)))
        (key nil))
  (when fig 
      (setf (marks fig) nil
	    (axes fig) nil 
            (cdr (assoc "scales" %context :test #'string=)) nil
            key (cdr (assoc "current-key" %context :test #'string=)))
      (when key
        (setf (cdr (assoc key (cdr (assoc "scale-registry" %context :test #'string=)))) nil))))
  (values))

;;needs to be checked 
(defun current-figure ()
  (unless (cdr (assoc "figure" %context :test #'string=))
    (figure)) ;;is the is the proper way to call the func
  (cdr (assoc "figure" %context :test #'string=)))


;(defun get-context ())

;(defun set-context (context))


(defun %fetch-axis (fig dimension scale)
  (let* ((axis-registry (getf fig :axis-registry))
    (dimension-data (getf axis-registry :dimension)))
     ;;;need the last two plus the try 
     ))

(defun update-fig-axis-registry (fig dimension scale axis)
  (let* (axis-registry (axis-registry fig)
    (dimension-scales (getf axis-registry :dimension nil)))
  (setf dimension-scales (append dimension-scales (list :scales "scales" :axis "axis"))) ;;not too sure 
  (setf (axis-registry fig) axis-registry)))
  
;(defun %get-attribute-dimension (trait-name &key (mark-type nil))
  ;(unless mark-type
    ;return-from %get-attribute-dimension) ;;is this the right way to return this  
  ;( ;;dont know the rest, for kevin 
      ;))

(defun %apply-properties (widget  &key (properties nil) &allow-other-keys)
  (with-pathname-p (hold-sync widget) ;;not very clear on how this portion works 
    (loop for (key . value)  in (items properties)
       do
	 (setf (slot-value widget key) value))))

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
#|

Altered version of %get-line-styles

(defun %get-line-styles (marker-str)
  (let ((ret-list nil))
    (loop for code-dict in (list line-style-codes color-codes marker-codes)
       do
	 (block outer
	   (loop for code in code-dict
	      do
		(when (search (car code) marker-str)
		  (setf ret-list (append ret-list (list (cdr code))))
		  (return-from outer nil)))))
    ret-list))

def _get_line_styles(marker_str):
    """Return line style, color and marker type from specified marker string.

    For example, if ``marker_Str`` is 'g-o' then the method returns
    ``('solid', 'green', 'circle')``.
    """
    ret_list = []
    for code_dict in [LINE_STYLE_CODES, COLOR_CODES, MARKER_CODES]:
        for code in code_dict:
            if code in marker_str:
                ret_list.append(code_dict[code])
                break
    return ret_list

|#
   



  


    
  


  





  
