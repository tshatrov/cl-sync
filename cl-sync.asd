;;;; -*- mode:lisp -*-

(use-package :asdf)

#+clisp
(defsystem cl-sync
    :name "cl-sync"
    :author "Timofei Shatrov <timofei.shatrov@gmail.com>"
    :description "Synchronizes directories"
    :components
    ((:file "sync-clisp"))
    :depends-on (:cl-fad :cl-ppcre))
#+sbcl
(defsystem cl-sync
    :name "cl-sync"
    :author "Timofei Shatrov <timofei.shatrov@gmail.com>"
    :description "Synchronizes directories"
    :components
    ((:file "sync-sbcl"))
    :depends-on (:cl-fad :cl-ppcre))
