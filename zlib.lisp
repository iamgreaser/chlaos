(defpackage :chen-zlib%
  (:use :cl :cffi))
(in-package :chen-zlib%)

(define-foreign-library zlib
  (:unix "libz.so"))
(use-foreign-library zlib)

(defctype bytef :uint8)
(defctype intf :int)
(defctype uintf :uint)
(defctype ulongf :ulong)
(defctype ulong :ulong)

;; ZEXTERN uLong ZEXPORT compressBound OF((uLong sourceLen));
(defcfun "compressBound" :ulong
  (source-len ulong))

;; ZEXTERN int ZEXPORT compress OF((Bytef *dest,   uLongf *destLen,
;;                                  const Bytef *source, uLong sourceLen));
(defcfun "compress" :int
  (dest (:pointer bytef))
  (dest-len (:pointer ulongf))
  (source (:pointer bytef)) ; FIXME: should be const
  (source-len ulong)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage :chen-zlib
  (:use :cl :cffi)
  (:export compress))
(in-package :chen-zlib)

(defun compress (indata)
  (let* ((inlen (length indata))
         (outlen (chen-zlib%::compressbound inlen)))
    (with-foreign-objects ((inbuf  'chen-zlib%::bytef inlen)
                           (outbuf 'chen-zlib%::bytef outlen)
                           (outbuf-len 'chen-zlib%::ulongf))
      (dotimes (i inlen)
        (setf (mem-aref inbuf 'chen-zlib%::bytef i)
              (aref indata i)))
      (setf (mem-ref outbuf-len 'chen-zlib%::ulongf) outlen)
      (chen-zlib%::compress outbuf outbuf-len inbuf inlen)
      (setf outlen (mem-ref outbuf-len 'chen-zlib%::ulongf))
      (let* ((outdata (make-array `(,outlen)
                                  :element-type '(unsigned-byte 8)
                                  :initial-element 0)))
        (dotimes (i outlen)
          (setf (aref outdata i)
                (mem-aref outbuf 'chen-zlib%::bytef i)))
        outdata))))

