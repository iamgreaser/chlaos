(ql:quickload :cffi)

(defpackage :enet
  (:use :cl :cffi :enet%))
(in-package :enet)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass enet-address ()
  ((host :accessor enet-address-host
         :initarg :host
         :initform enet%::+enet-host-any+)
   (port :accessor enet-address-port
         :initarg :port
         :initform enet%::+enet-port-any+)))

(defun make-enet-address (&key host port)
  (make-instance 'enet-address
                 :host (or host enet%::+enet-host-any+)
                 :port (or port enet%::+enet-port-any+)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
