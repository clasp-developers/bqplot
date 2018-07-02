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
  (remove key kwargs)
  (remove ':key kwargs)
  (remove ':fig kwargs)
  (remove fig kwargs)
  ;;;Now begins the translation of python code.
  (let ((scales-arg (getf kwargs ':scales)))
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
	  ))))
;;;Don't know what ^ is


;;;Starting to work on the plot method
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

;;;Helper function for plot
(defun strip (string)
  (string-trim #(#\Space #\Newline #\Return) string))

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
    (loop for name in (list "x" "y" "z")
       do
         (let ((dimension (%get-attribute-dimension name mark-type)))
           (cond ((not (getf kwargs (intern name "KEYWORD")))
                  (values))
                 ((getf scales (intern name "KEYWORD"))
                  (when update-context
                    (setf (cdr (assoc dimension (cdr (assoc "scales" %context :test #'string=)) :test #'string=)) (cdr (assoc name scales :test #'string=)))))
                 ;;;Need to address (elif dimension not in _context['scales']: ...What is dimension here?
                 (t
                  (if (assoc name scales :test #'string=)
                      (setf (cdr (assoc name scales :test #'string=)) (cdr (assoc dimension (cdr (assoc "scales" :test #'string=)) :test #'string=)))
                      (push (cons name (cdr (assoc dimension (cdr (assoc "scales" :test #'string=)) :test #'string))) scales))))))
    (setf mark (mark-type :scales scales kwargs)
          (cdr (assoc "last-mark" %context :test #'string=)) mark
          (marks fig) (concatenate 'list (marks fig) (list mark)))
    (axes mark :options axes-options)
    mark))



    
(defun %get-attribute-dimension (trait-name &optional mark-type)
  (unless mark-type
    (return-from %get-attribute-dimension trait-name))
  (let ((scale-metadata (scales-metadata mark-type)))
    (cdr (assoc "dimension" (cdr (assoc trait-name scale-metadata :test #'string=)) :test #'string=)))) 
