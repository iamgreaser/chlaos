(in-package :chenthread.chlaos)

(declaim (optimize (compilation-speed 0)
                   (debug 3)
                   (speed 3)
                   (space 1)
                   (safety 3)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant +aos-disconnect-error-undefined+ 0)
(defconstant +aos-disconnect-error-banned+ 1)
(defconstant +aos-disconnect-error-kicked+ 2)
(defconstant +aos-disconnect-error-wrong-version+ 3)
(defconstant +aos-disconnect-error-full+ 4)

(defvar *server-up* nil)
(defvar *server-stopping* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TEST

(defvar *peers* (list))
(defclass peer ()
  ((epeer :type enet%::enet-peer
          :reader peer-epeer
          :initarg :epeer)
   ))

(defun compare-peer-matcher (epeer)
  (lambda (peer)
    (eql (peer-epeer peer)
         epeer)))

(defun find-peer (epeer)
  (car (member-if (compare-peer-matcher epeer)
                  *peers*)))

(defun add-peer (epeer)
  (unless (find-peer epeer)
    (format t "Adding peer~%")
    (push (make-instance 'peer
                         :epeer epeer)
          *peers*)
    (assert (find-peer epeer))))

(defun remove-peer (epeer)
  (setf *peers* (remove (find-peer epeer) *peers*)))

(defun server-loop (host event)
  (when (null-pointer-p host)
    (error "Failed to allocate host"))
  (setf *peers* (list))
  (enet%::enet-host-compress-with-range-coder host)
  (format t "Waiting for connection...~%")
  (do ((svcresult 0 (enet%::enet-host-service host event 5)))
      ((or *server-stopping* (< svcresult 0)))
    (when (> svcresult 0)
      (with-foreign-slots ((enet%::type
                            enet%::packet
                            enet%::data
                            enet%::peer)
                           event
                           (:struct enet%::enet-event))
        (ecase enet%::type
          (:connect
           (format t "Connecting new client~%")
           (format t "- Data: ~8,'0X~%" enet%::data)
           (case enet%::data
             (4 (format t "AoS 0.76 detected, good man~%")
                (add-peer enet%::peer))
             (3 (format t "AoS 0.75 detected, what a fucking pleb~%")
                (enet%::enet-peer-disconnect enet%::peer +aos-disconnect-error-wrong-version+))
             (t (format t "Unknown version detected, kick the client out~%")
                (enet%::enet-peer-disconnect enet%::peer +aos-disconnect-error-wrong-version+))))
          (:disconnect
           (format t "Disconnecting client~%")
           (remove-peer enet%::peer))
          (:receive
           (with-foreign-slots ((enet%::data enet%::data-length) enet%::packet (:struct enet%::enet-packet))
             (format t "Data received: ~a bytes~%"
                     enet%::data-length))
           (enet%::enet-packet-destroy enet%::packet)
           ))))))

(defun run-server ()
  (with-foreign-objects ((host '(:pointer (:struct enet%::enet-host)))
                         (address '(:struct enet%::enet-address))
                         (event '(:struct enet%::enet-event)))
    (with-foreign-slots ((enet%::host enet%::port) address (:struct enet%::enet-address))
      (setf enet%::host enet%::+enet-host-any+
            enet%::port 32887))
    (setf host (enet%::enet-host-create address 64 2 0 0))
    (assert (not *server-up*))
    (setf *server-up* t)
    (setf *server-stopping* nil)
    (unwind-protect 
         (server-loop host event)
      (format t "Connection closed~%")
      (enet%::enet-host-destroy host)
      (setf *server-up* nil)
      (setf *server-stopping* nil)
      ;; FIXME: destroy peers when done
      )))

(defun stop-server ()
  (setf *server-stopping* t))

(run-server)
