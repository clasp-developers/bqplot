(asdf:defsystem #:cl-bqplot
    :description "A widget library for graphing, based on Bloomberg's BQPLOT."
    :version "0.1"
    :author "Kevin Esslinger"
    :license "LGPL2. See LICENSE."
  :depends-on (:cl-jupyter
               :cl-ipywidgets)
    :serial t
    :components (
                 (:file "packages")
                 (:file "frontend")
                 (:file "colorschemes")
                 (:file "figure")
                 (:file "marks")
                 (:file "pyplot")
                 (:file "scales")
                 (:file "axes")
                 (:file "default_tooltip")
                 (:file "toolbar")
                 (:file "market_map")
                 (:file "interacts")
                 ))

