(in-package :chenthread.chlaos)

(declaim (optimize (compilation-speed 0)
                   (debug 3)
                   (speed 3)
                   (space 1)
                   (safety 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass vxl ()
  ((columns :type (simple-array
                   (or null (simple-array (unsigned-byte 8) (*)))
                   (* *))
            :initform (make-array
                       '(512 512)
                       :initial-element nil
                       :element-type '(or null
                                       (simple-array
                                        (unsigned-byte 8)
                                        (*)))))
   (default-column :type (simple-array (unsigned-byte 8) (*))
     :reader vxl-default-column
     :initform (make-array '(8)
                           :element-type '(unsigned-byte 8)
                           :initial-contents
                           '(0 63 63 0 255 0 255 128)))
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun grab-4-bytes-reversed-as-list (stream)
  (let* ((v0 (read-byte stream))
         (v1 (read-byte stream))
         (v2 (read-byte stream))
         (v3 (read-byte stream)))
    (declare (type (unsigned-byte 8) v0 v1 v2 v3))
    (list v3 v2 v1 v0)))

(defun load-vxl-from-stream (stream)
  (let* ((vxl (make-instance 'vxl)))
    (with-slots (columns) vxl
      (dotimes (y 512)
        (dotimes (x 512)
          (let* ((header nil)
                 (acc (list))
                 (steps-until-header-check 0)
                 (is-last-header nil))
            (tagbody
             loop-body
               (setf header (grab-4-bytes-reversed-as-list stream))
               (setf acc (append header acc))
               ;(assert (>= steps-until-header-check 0))
               (let* ((n (nth 3 acc))
                      (s (nth 2 acc))
                      (e (nth 1 acc)))
                 ;(format t "nse ~a ~a ~a ~s~%" n s e acc)
                 (if (= n 0)
                     (setf steps-until-header-check (max 0 (- e s -1))
                           is-last-header t)
                     (setf steps-until-header-check (1- n))))
               (dotimes (step steps-until-header-check)
                 (setf acc (append (grab-4-bytes-reversed-as-list stream)
                                   acc)))
               (unless is-last-header
                 (go loop-body)))
            (setf (aref columns y x)
                  (make-array `(,(length acc))
                              :element-type '(unsigned-byte 8)
                              :initial-contents (reverse acc)))
            ))))
    vxl))

(defun load-vxl-from-file-name (file-name)
  (with-open-file (stream file-name
                          :direction :input
                          :element-type '(unsigned-byte 8))
    (load-vxl-from-stream stream)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun save-vxl-to-byte-array (vxl)
  (let* ((output (make-array `(10)
                             :element-type '(unsigned-byte 8)
                             :adjustable t
                             :fill-pointer 0)))
    (macrolet ((w (n) `(vector-push-extend ,n output)))
      (dotimes (y 512)
        (dotimes (x 512)
          (let* ((column (vxl-column vxl x y)))
            (dotimes (i (length column))
              (w (aref column i)))))))
    output))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric vxl-column (vxl x y))
(defgeneric (setf vxl-column) (column vxl x y))
(defmethod vxl-column ((vxl vxl) x y)
  (with-slots (columns default-column) vxl
    (let* ((column (or (aref columns x y)
                       default-column)))
      (declare (type (simple-array (unsigned-byte 8) (*)) column))
      column)))
(defmethod (setf vxl-column) (column (vxl vxl) x y)
  (with-slots (columns) vxl
    (setf (aref columns x y) column))
  column)

(defun get-4-bytes-as-list (column offs)
  (list (aref column (+ offs 0))
        (aref column (+ offs 1))
        (aref column (+ offs 2))
        (aref column (+ offs 3))))

(defun get-column-cell (column z)
  (block nil
    (let* ((header-offs 0))
      (tagbody
       loop-body
         (let* ((n (aref column (+ header-offs 0)))
                (s (aref column (+ header-offs 1)))
                (e (aref column (+ header-offs 2))))

           (when (< z s)
             (return-from nil nil))
           (when (<= z e)
             (return-from nil
               (get-4-bytes-as-list column
                                    (+ header-offs
                                       (* 4 (+ 1 (- z s)))))))

           (when (= n 0)
             (return-from nil t))
           (incf header-offs (* n 4))

           (let* ((a2 (aref column (+ header-offs 3)))
                  (c (- (1- n) (1+ (- e s))))
                  (cs (- a2 c))
                  (ci (- z a2)))
             (when (< z cs)
               (return-from nil t))
             (when (< z a2)
               (return-from nil
                 (get-4-bytes-as-list column
                                      (+ header-offs (* 4 ci)))))
             (go loop-body)))))))

;; doin' it the lazy way lol
;; true enterprise coding here
(defun unpack-column (column)
  (let* ((unpacked (make-array `(,64)
                               :initial-element nil)))
    (dotimes (z (array-dimension unpacked 0))
      (setf (aref unpacked z)
            (get-column-cell column z)))
    unpacked))

;; can't do this the lazy way though
(defun pack-column (unpacked)
  (let* ((output-elems (list))
         (floor-cells (list))
         (ceiling-cells (list))
         (floor-start nil)
         (floor-end nil)
         (air-start 0))
    (dotimes (z 64)
      (let* ((cell (aref unpacked z)))
        (cond
          ;; Waiting for air run to finish
          ((and (null cell) (null floor-start)) t)

          ;; End of air run, start of floor run
          ((and (consp cell) (null floor-start))
           (setf floor-start z)
           (setf floor-cells (append floor-cells cell)))

          ;; Continue floor run
          ((and (consp cell) floor-start (null floor-end))
           (setf floor-cells (append floor-cells cell)))

          ;; End of floor run, start of OR continue dirt run
          ((and (eq t cell) floor-start)
           (unless floor-end
             (setf floor-end (1- z))))

          ;; End of dirt run, start of ceiling run
          ((and (consp cell) floor-start floor-end)
           (setf ceiling-cells (append ceiling-cells cell)))

          ;; End of ceiling run OR floor run, start of air run
          ((and (null cell) floor-start)
           (unless floor-end
             (setf floor-end (1- z)))
           (assert floor-start)
           (assert floor-end)
           (assert air-start)
           (setf output-elems
                 (append output-elems
                         `(,(+ (/ (length floor-cells)   4)
                               (/ (length ceiling-cells) 4)
                               1)
                            ,floor-start
                            ,floor-end
                            ,air-start)
                         floor-cells
                         ceiling-cells))
           (setf air-start z)
           (setf floor-start nil)
           (setf floor-end nil)
           (setf floor-cells (list))
           (setf ceiling-cells (list)))
          (t (error (format nil "TODO: Handle case Z=~s CELL=~s H=~s ~s"
                            z cell floor-start floor-end))))))

    (unless floor-end
      (setf floor-end (1- 64)))
    (assert floor-start)
    (assert floor-end)
    (assert air-start)
    (setf output-elems
          (append output-elems
                  `(,0
                    ,floor-start
                    ,floor-end
                    ,air-start)
                  floor-cells
                  ceiling-cells))
    (make-array `(,(length output-elems))
                :element-type '(unsigned-byte 8)
                :initial-contents output-elems)))


(defgeneric vxl-cell (vxl x y z))
(defgeneric (setf vxl-cell) (cell vxl x y z))
(defmethod vxl-cell ((vxl vxl) x y z)
  (let* ((column (vxl-column vxl x y)))
    (get-column-cell column z)))
(defmethod (setf vxl-cell) (cell (vxl vxl) x y z)
  (let* ((column (vxl-column vxl x y))
         (unpacked (unpack-column column)))
    ;(format t "~s~%" unpacked)
    (setf (aref unpacked z) cell)
    (let* ((packed (pack-column unpacked)))
      ;(format t "~s~%" packed)
      (setf (vxl-column vxl x y) packed)))
  cell)

;; Tests

;; Ensure that the base case is correct.
(let* ((vxl (make-instance 'vxl)))
  (assert (equalp (vxl-column vxl 0 0)
                  #(0 63 63 0 255 0 255 128)))
  (assert (equalp (vxl-column vxl 1 0)
                  #(0 63 63 0 255 0 255 128)))
  (setf (vxl-cell vxl 0 0 63) `(32 32 32 128))
  (assert (equalp (vxl-column vxl 0 0)
                  #(0 63 63 0 32 32 32 128)))
  (assert (equalp (vxl-column vxl 1 0)
                  #(0 63 63 0 255 0 255 128)))
  )

(let* ((column #(0 62 62 0
                 255 255 255 128)))
  (assert (equalp (pack-column
                   (unpack-column column))
                  column))
  (apply #'mapcar
         (lambda (z ref)
           (assert (equal (get-column-cell column z) ref)))
         (apply #'mapcar #'list
                '((3 nil)
                  (61 nil)
                  (62 (255 255 255 128))
                  (63 t)))))
                  
(let* ((column #(2 60 60 0
                 128 128 128 128
                 0 62 62 61
                 255 255 255 128)))
  (assert (equalp (pack-column
                   (unpack-column column))
                  column))
  (apply #'mapcar
         (lambda (z ref)
           (assert (equal (get-column-cell column z) ref)))
         (apply #'mapcar #'list
                '((3 nil)
                  (59 nil)
                  (60 (128 128 128 128))
                  (61 nil)
                  (62 (255 255 255 128))
                  (63 t)))))
                  
(let* ((column #(4 57 58 0
                 128 128 128 128
                 32 32 32 128
                 64 64 64 128
                 0 62 62 61
                 255 255 255 128)))
  (assert (equalp (pack-column
                   (unpack-column column))
                  column))
  (apply #'mapcar
         (lambda (z ref)
           (assert (equal (get-column-cell column z) ref)))
         (apply #'mapcar #'list
                '((3 nil)
                  (56 nil)
                  (57 (128 128 128 128))
                  (58 (32 32 32 128))
                  (59 t)
                  (60 (64 64 64 128))
                  (61 nil)
                  (62 (255 255 255 128))
                  (63 t)))))
                  
(let* ((column #(3 57 57 0
                 128 128 128 128
                 64 64 64 128
                 0 62 62 61
                 255 255 255 128)))
  (assert (equalp (pack-column
                   (unpack-column column))
                  column))
  (apply #'mapcar
         (lambda (z ref)
           (assert (equal (get-column-cell column z) ref)))
         (apply #'mapcar #'list
                '((3 nil)
                  (56 nil)
                  (57 (128 128 128 128))
                  (58 t)
                  (59 t)
                  (60 (64 64 64 128))
                  (61 nil)
                  (62 (255 255 255 128))
                  (63 t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; no map: 69116
;; with map: 79972
;;
;; note that these fluctuate a bit
