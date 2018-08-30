;; until I know how to use ASDF this will have to do

(defpackage :chenthread.chlaos
  (:use :cl
        :alexandria
        :bordeaux-threads
        :cffi

        :enet

        ;:usocket
        ))

(declaim (optimize (compilation-speed 0)
                   (debug 3)
                   (speed 3)
                   (space 1)
                   (safety 3)))

