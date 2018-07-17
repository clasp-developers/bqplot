(asdf:defsystem #:cl-bqplot
    :description "The ngl widget for cl-jupyter with widgets"
    :version "0.1"
    :author "Kevin Esslinger"
    :license "LGPL2. See LICENSE."
  :depends-on (:cl-jupyter
               :cl-jupyter-widgets)
    :serial t
    :components (
                 (:file "packages")
                 (:file "frontend")
                 (:file "colorschemes")
                 (:file "figure")
                 (:file "marks")
                 (:file "pyplot")
                 (:file "scales")
                 ))
