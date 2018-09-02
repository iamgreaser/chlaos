(in-package :chenthread.chlaos)

(declaim (optimize (compilation-speed 0)
                   (debug 3)
                   (speed 3)
                   (space 1)
                   (safety 3)))


(defparameter *update-timeout-ms* 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant +aos-disconnect-error-undefined+ 0)
(defconstant +aos-disconnect-error-banned+ 1)
(defconstant +aos-disconnect-error-kicked+ 2)
(defconstant +aos-disconnect-error-wrong-version+ 3)
(defconstant +aos-disconnect-error-full+ 4)

(defvar *server-up* nil)
(defvar *server-stopping* nil)
(defvar *server-thread* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TEST

(defvar *peers* (list))
(defvar *map* nil)

(defclass peer ()
  ((epeer :reader peer-epeer
          ;; FIXME: needs CFFI type
          ;; :type (:struct enet%::enet-peer)
          :initarg :epeer)
   (map-data-to-send :accessor peer-map-data-to-send
                     :type (or null (simple-array (unsigned-byte 8)))
                     :initarg :map-data-to-send
                     :initform nil)
   (map-data-send-offset :accessor peer-map-data-send-offset
                         :type (or null integer)
                         :initform nil)
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass field-type ()
  ((name :initarg :name :reader field-type-name)
   (base :initarg :base :reader field-type-base)
   (writer :initarg :writer :reader field-type-writer)
   ))

(defparameter *field-types*
  (mapcar (lambda (x)
            (destructuring-bind (name base writer) x
              `(,name
                ,(make-instance 'field-type
                                :name name
                                :base base
                                :writer writer))))
          `(;;
            (u8 (unsigned-byte 8)
                ,(lambda (v array)
                   (declare (type (unsigned-byte 8) v))
                   (vector-push-extend v array)))
            (s8 (signed-byte 8)
                ,(lambda (v array)
                   (declare (type (signed-byte 8) v))
                   (vector-push-extend (logand #xFF v) array)))
            (u16 (unsigned-byte 16)
                 ,(lambda (v array)
                    (declare (type (unsigned-byte 16) v))
                    (vector-push-extend (logand #xFF v) array)
                    (vector-push-extend (logand #xFF (ash v -8)) array)
                    ))
            (s16 (signed-byte 16)
                 ,(lambda (v array)
                    (declare (type (signed-byte 16) v))
                    (vector-push-extend (logand #xFF v) array)
                    (vector-push-extend (logand #xFF (ash v -8)) array)
                    ))
            (u32 (unsigned-byte 32)
                 ,(lambda (v array)
                    (declare (type (unsigned-byte 32) v))
                    (vector-push-extend (logand #xFF v) array)
                    (vector-push-extend (logand #xFF (ash v -8)) array)
                    (vector-push-extend (logand #xFF (ash v -16)) array)
                    (vector-push-extend (logand #xFF (ash v -24)) array)
                    ))
            (s32 (signed-byte 32)
                 ,(lambda (v array)
                    (declare (type (signed-byte 32) v))
                    (vector-push-extend (logand #xFF v) array)
                    (vector-push-extend (logand #xFF (ash v -8)) array)
                    (vector-push-extend (logand #xFF (ash v -16)) array)
                    (vector-push-extend (logand #xFF (ash v -24)) array)
                    ))
            (byte-array (simple-array (unsigned-byte 8) (*))
                ,(lambda (v array)
                   (declare (type (simple-array (unsigned-byte 8) (*)) v))
                   (dotimes (i (length v))
                     (vector-push-extend (aref v i) array))))
            )))

(defun lookup-field (name)
  (second (assoc name *field-types*)))

(defun lookup-field-type-base (name)
  (field-type-base (lookup-field name)))
(defun lookup-field-type-writer (name)
  (field-type-writer (lookup-field name)))

(defmacro make-packet-type (id name &body fields)
  (let* ((s-make-x-packet (intern (format nil "MAKE-~a-PACKET" name)))
         (field-names (mapcar #'first fields))
         (s-outbuf (gensym))
         )
    `(defun ,s-make-x-packet (&key ,@field-names)
       ,@(mapcar (lambda (x)
                   (destructuring-bind (name type) x
                     `(declare (type ,(lookup-field-type-base type) ,name))))
                 fields)
       ,@(mapcar (lambda (x) `(assert ,x))
                 field-names)
       (let* ((,s-outbuf (make-array `(10)
                                     :element-type '(unsigned-byte 8)
                                     :adjustable t
                                     :fill-pointer 0)))
         (vector-push-extend ,id ,s-outbuf)
         ,@(mapcar (lambda (x)
                     (destructuring-bind (name type) x
                       `(funcall
                         (lookup-field-type-writer ',type)
                         ,name ,s-outbuf)))
                   fields)
         ,s-outbuf)
       )))

(progn
  (make-packet-type 18 map-start
    (map-length u32))
  (make-packet-type 19 map-data
    (data byte-array)))

;; TESTS

(progn
  (assert (equalp (make-map-start-packet :map-length 1025)
                  #(18 1 4 0 0)))
  (assert (equalp (make-map-data-packet
                   :data (make-array `(5)
                                     :element-type '(unsigned-byte 8)
                                     :initial-contents #(1 2 3 4 5)))
                  #(19 1 2 3 4 5))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun compare-peer-matcher (epeer)
  (lambda (peer)
    (pointer-eq (peer-epeer peer)
                epeer)))

(defun find-peer (epeer)
  (car (member-if (compare-peer-matcher epeer)
                  *peers*)))

(defun add-peer (epeer)
  (unless (find-peer epeer)
    (format t "Adding peer~%")
    (let* ((peer (make-instance 'peer
                                :epeer epeer)))
      (push peer *peers*)
      (assert (eql (find-peer epeer) peer))
      peer)))

(defun remove-peer (epeer)
  (setf *peers* (remove (find-peer epeer) *peers*)))

(defgeneric send-map-to-peer (peer vxl))
(defgeneric send-packet-to-peer (peer data &key &allow-other-keys))
(defgeneric tick-peer (peer))

(defmethod send-packet-to-peer ((peer peer) data &key &allow-other-keys)
  ;(format t "send ~a ~a~%" (aref data 0) (length data))
  (with-foreign-objects ((packet '(:pointer (:struct enet%::enet-packet)))
                         (outbuf :uint8 (length data)))
    (dotimes (i (length data))
      (setf (mem-aref outbuf :uint8 i)
            (aref data i)))
    (setf packet (enet%::enet-packet-create
                  outbuf (length data)
                  enet%::+enet-packet-flag-reliable+))
    (enet%::enet-peer-send (peer-epeer peer) 0 packet)
    ))

(defmethod send-map-to-peer ((peer peer) (vxl vxl))
  (time
   (progn
     (format t "Preparing sending of map ~a to peer ~a...~%"
             vxl peer)
     (setf (peer-map-data-to-send peer)
           (chen-zlib:compress
            (save-vxl-to-byte-array vxl))))))

(defmethod tick-peer ((peer peer))
  (cond ((peer-map-data-to-send peer)
         ;; We need to send the map.
         (symbol-macrolet ((map-data (peer-map-data-to-send peer))
                           (map-offset (peer-map-data-send-offset peer)))
           (let* ((map-length (length map-data))
                  (chunk-length 1024)
                  (chunk-end (when map-offset
                               (min (+ map-offset chunk-length)
                                    map-length))))
             (cond ((null map-offset)
                    ;; Haven't started sending this.
                    ;; Send a map-start packet.
                    ;; TODO: CRC32 and map name
                    (send-packet-to-peer
                     peer
                     (make-map-start-packet :map-length map-length))
                    (setf map-offset 0))

                   ((>= map-offset (length map-data))
                    ;; We've run out of map data.
                    ;; Send a... I forget what packet.
                    ;; No more map data needs to be sent.
                    (setf map-data nil)
                    (setf map-offset nil)
                    ;;(send-packet-to-peer
                    ;; (make-map-end-packet)
                    ;; peer)
                    )

                   (t
                    ;; Normal case.
                    ;; Send what we can and advance.
                    (send-packet-to-peer
                     peer
                     (make-map-data-packet
                      :data (subseq map-data map-offset chunk-end)))
                    (setf map-offset chunk-end))

                   )))
         )))

(defun tick-server-peer (peer)
  (tick-peer peer))

(defun tick-server-logic ()
  (dolist (peer *peers*)
    (tick-server-peer peer))
  )

(defun server-loop (host event)
  (when (null-pointer-p host)
    (error "Failed to allocate host"))
  (setf *peers* (list))
  (enet%::enet-host-compress-with-range-coder host)
  (format t "Waiting for connection...~%")
  (do ((svcresult 0 (enet%::enet-host-service host event *update-timeout-ms*)))
      ((or *server-stopping* (< svcresult 0)))
    (if (> svcresult 0)
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
                  (let* ((peer (add-peer enet%::peer)))
                    (send-map-to-peer peer *map*)
                    ))
               (3 (format t "AoS 0.75 detected, what a fucking pleb~%")
                  (enet%::enet-peer-disconnect enet%::peer +aos-disconnect-error-wrong-version+))
               (t (format t "Unknown version detected, kick the client out~%")
                  (enet%::enet-peer-disconnect enet%::peer +aos-disconnect-error-wrong-version+))))
            (:disconnect
             (format t "Disconnecting client~%")
             (remove-peer enet%::peer))
            (:receive
             (with-foreign-slots ((enet%::data enet%::data-length)
                                  enet%::packet
                                  (:struct enet%::enet-packet))
               (format t "Data received: ~a bytes~%"
                       enet%::data-length))
             (enet%::enet-packet-destroy enet%::packet)
             )))
        (tick-server-logic))))

(defun run-server-main ()
  (with-foreign-objects ((host '(:pointer (:struct enet%::enet-host)))
                         (address '(:struct enet%::enet-address))
                         (event '(:struct enet%::enet-event)))
    (with-foreign-slots ((enet%::host enet%::port) address (:struct enet%::enet-address))
      (setf enet%::host enet%::+enet-host-any+
            enet%::port 32887))
    (setf host (enet%::enet-host-create address 64 2 0 0))
    (setf *server-stopping* nil)
    (unwind-protect 
         (server-loop host event)
      (unwind-protect
           (progn
             (format t "Disconnecting peers...~%")
             (dolist (peer *peers*)
               (enet%::enet-peer-disconnect
                (peer-epeer peer)
                +aos-disconnect-error-full+)))
        (format t "Connection closed~%")
        (enet%::enet-host-destroy host)
        (setf *server-up* nil)
        (setf *server-stopping* nil)
        (setf *peers* nil)
        ))))

(defun run-server ()
  (assert (not *server-up*))
  (setf *server-up* t)
  (let* ((ios (list *standard-input*
                    *standard-output*
                    *error-output*
                    *trace-output*
                    *query-io*
                    *debug-io*)))
    (setf *server-thread*
          (bordeaux-threads:make-thread
           (lambda ()
             (destructuring-bind (*standard-input*
                                  *standard-output*
                                  *error-output*
                                  *trace-output*
                                  *query-io*
                                  *debug-io*)
                 ios
               (run-server-main)))
           :name "chlaos-main"))))

(defun stop-server ()
  (setf *server-stopping* t))

(defun load-server-map (fname)
  (time (progn
          (format t "Loading map ~s...~%" fname)
          (setf *map* (load-vxl-from-file-name fname)))))

(load-server-map "mesa.vxl")
(stop-server)
(run-server)
