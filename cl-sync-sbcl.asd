;;;; -*- mode:lisp -*-

(use-package :asdf)

(defsystem cl-sync-sbcl
    :name "cl-sync"
    :author "Timofei Shatrov <timofei.shatrov@gmail.com>"
    :description "Synchronizes directories"
    :components
    ((:file "sync-sbcl"))
    :depends-on (:cl-fad :cl-ppcre))
