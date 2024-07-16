(asdf:defsystem :regis
  :depends-on (#:usocket
               #:alexandria
               #:bordeaux-threads
               #:cl-ppcre
               #:xpath
               #:cxml-stp)
  :components ((:file "regis")
               (:file "svg")))
