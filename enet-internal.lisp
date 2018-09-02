(defpackage :enet%
  (:use :cl :cffi))
(in-package :enet%)

(define-foreign-library libenet
  (:unix "libenet.so"))

(use-foreign-library libenet)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; FIXME: CFFI doesn't appear to define size_t so we have to guess.
;; long matches at least for x86 and amd64 (at least in theory).
(defctype size-t :ulong)
(defctype ssize-t :long)

(defctype enet-uint8 :uint8)
(defctype enet-uint16 :uint16)
(defctype enet-uint32 :uint32)

(defctype enet-version enet-uint32)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; enet/unix.h stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FIXME: have the non-UNIX versions of these too
(defctype enet-socket :int)
(defcstruct enet-buffer
  (data (:pointer :void))
  (data-length size-t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; enet/protocol.h constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant +enet-protocol-minimum-mtu+ 576)
(defconstant +enet-protocol-maximum-mtu+ 4096)
(defconstant +enet-protocol-maximum-packet-commands+ 32)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; enet/enet.h constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; FIXME: some UNIX systems define MSG_MAXIOVLEN and ENet uses that instead.
;; I don't know which ones these are though. Linux doesn't do this.
;; Hopefully it's only Solaris that does this if it even does it.
(defconstant +enet-buffer-maximum+
  (1+ (* 2 +enet-protocol-maximum-packet-commands+)))

(defconstant +enet-peer-unsequenced-window-size+ 1024)
(defconstant +enet-peer-reliable-windows+ 16)

;; not sure where these are actually defined
(defconstant +enet-host-any+ 0)
(defconstant +enet-host-broadcast+ #xFFFFFFFF)
(defconstant +enet-port-any+ 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; enet/enet.h enums and bitmasks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcenum enet-socket-type (:stream 1) (:datagram 2))
;(defcenum enet-socket-wait ...)
;(defctype enet-socket-waits enet-uint32)
(defcenum enet-socket-option
  (:nonblock 1)
  (:broadcast 2)
  (:rcvbuf 3)
  (:sndbuf 4)
  (:reuseaddr 5)
  (:rcvtimeo 6)
  (:sndtimeo 7)
  (:error 8)
  (:nodelay 9)
  )
(defcenum enet-socket-shutdown (:read 0) (:write 1) (:read-write 2))
;(defcenum enet-packet-flag ...)

(defconstant +enet-packet-flag-reliable+ (ash 1 0))
(defconstant +enet-packet-flag-unsequenced+ (ash 1 1))
(defconstant +enet-packet-flag-no-allocate+ (ash 1 2))
(defconstant +enet-packet-flag-unreliable-fragment+ (ash 1 3))

(defctype enet-packet-flags enet-uint32)

(defcenum enet-peer-state
  (:disconnected 0)
  (:connecting 1)
  (:acknowledging-connect 2)
  (:connection-pending 3)
  (:connection-succeeded 4)
  (:connected 5)
  (:disconnect-later 6)
  (:disconnecting 7)
  (:acknowledging-disconnect 8)
  (:zombie 9)
  )

(defcenum enet-event-type
  (:none 0)
  (:connect 1)
  (:disconnect 2)
  (:receive 3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; enet/list.h structs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcstruct enet-list-node
  (next (:pointer (:struct enet-list-node)))
  (previous (:pointer (:struct enet-list-node)))
  )

(defcstruct enet-list
  (sentinel (:struct enet-list-node))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; enet/protocol.h structs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; These are all packed structures.

(defcstruct (enet-protocol-header :size 4))
(defcstruct (enet-protocol-command-header :size 4))
(defcstruct (enet-protocol-acknowledge :size 8))
(defcstruct (enet-protocol-connect :size 48))
(defcstruct (enet-protocol-verify-connect :size 44))
(defcstruct (enet-protocol-bandwidth-limit :size 12))
(defcstruct (enet-protocol-throttle-configure :size 16))
(defcstruct (enet-protocol-disconnect :size 8))
(defcstruct (enet-protocol-ping :size 4))
(defcstruct (enet-protocol-send-reliable :size 6))
(defcstruct (enet-protocol-send-unreliable :size 8))
(defcstruct (enet-protocol-send-unsequenced :size 8))
(defcstruct (enet-protocol-send-fragment :size 24))
(defcunion enet-protocol
  (header (:struct enet-protocol-header))
  (acknowledge (:struct enet-protocol-acknowledge))
  (connect (:struct enet-protocol-connect))
  (verify-connect (:struct enet-protocol-verify-connect))
  (disconnect (:struct enet-protocol-disconnect))
  (ping (:struct enet-protocol-ping))
  (send-reliable (:struct enet-protocol-send-reliable))
  (send-unreliable (:struct enet-protocol-send-unreliable))
  (send-unsequenced (:struct enet-protocol-send-unsequenced))
  (send-fragment (:struct enet-protocol-send-fragment))
  (bandwidth-limit (:struct enet-protocol-bandwidth-limit))
  (throttle-configure (:struct enet-protocol-throttle-configure))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; enet/enet.h structs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcstruct enet-address
  (host enet-uint32)
  (port enet-uint16))

(defcstruct enet-packet
  (reference-count size-t)
  (flags enet-packet-flags)
  (data (:pointer enet-uint8))
  (data-length size-t)
  (free-callback :pointer) ; TODO: assign C type ENetPacketFreeCallback
  (user-data (:pointer :void))
  )

(defcstruct enet-acknowledgement
  (acknowledgement-list (:struct enet-list-node))
  (sent-time enet-uint32)
  (command (:union enet-protocol)))

(defcstruct enet-outgoing-command
  (outgoing-command-list (:struct enet-list-node))
  (reliable-sequence-number enet-uint16)
  (unreliable-sequence-number enet-uint16)
  (sent-time enet-uint32)
  (round-trip-timeout enet-uint32)
  (round-trip-timeout-limit enet-uint32)
  (fragment-offset enet-uint32)
  (fragment-length enet-uint16)
  (send-attempts enet-uint16)
  (command (:union enet-protocol))
  (packet (:pointer (:struct enet-packet)))
  )

(defcstruct enet-incoming-command
  (incoming-command-list (:struct enet-list-node))
  (reliable-sequence-number enet-uint16)
  (unreliable-sequence-number enet-uint16)
  (command (:union enet-protocol))
  (fragment-count enet-uint32)
  (fragments-remaining enet-uint32)
  (fragments (:pointer enet-uint32))
  (packet (:pointer (:struct enet-packet)))
  )

(defcstruct enet-channel
  (outgoing-reliable-sequence-number enet-uint16)
  (outgoing-unreliable-sequence-number enet-uint16)
  (used-reliable-windows enet-uint16)
  (reliable-windows (:array enet-uint16 #.+enet-peer-reliable-windows+))
  (incoming-reliable-sequence-number enet-uint16)
  (incoming-unreliable-sequence-number enet-uint16)
  (incoming-reliable-commands (:struct enet-list))
  (incoming-unreliable-commands (:struct enet-list))
  )

(defcstruct enet-host)

(defcstruct enet-peer
  (dispatch-list (:struct enet-list-node))
  (host (:pointer (:struct enet-host)))
  (outgoing-peer-id enet-uint16)
  (incoming-peer-id enet-uint16)
  (connect-id enet-uint32)
  (outgoing-session-id enet-uint8)
  (incoming-session-id enet-uint8)
  (address (:struct enet-address))
  (data (:pointer :void))
  (state enet-peer-state)
  (channels (:pointer (:struct enet-channel)))
  (channel-count size-t)
  (incoming-bandwidth enet-uint32)
  (outgoing-bandwidth enet-uint32)
  (incoming-bandwidth-throttle-epoch enet-uint32)
  (outgoing-bandwidth-throttle-epoch enet-uint32)
  (incoming-data-total enet-uint32)
  (outgoing-data-total enet-uint32)
  (last-send-time enet-uint32)
  (last-receive-time enet-uint32)
  (next-timeout enet-uint32)
  (earliest-timeout enet-uint32)
  (packet-loss-epoch enet-uint32)
  (packets-sent enet-uint32)
  (packets-lost enet-uint32)
  (packet-loss enet-uint32)
  (packet-loss-variance enet-uint32)
  (packet-throttle enet-uint32)
  (packet-throttle-limit enet-uint32)
  (packet-throttle-counter enet-uint32)
  (packet-throttle-epoch enet-uint32)
  (packet-throttle-acceleration enet-uint32)
  (packet-throttle-deceleration enet-uint32)
  (packet-throttle-interval enet-uint32)
  (ping-interval enet-uint32)
  (timeout-limit enet-uint32)
  (timeout-minimum enet-uint32)
  (timeout-maximum enet-uint32)
  (last-round-trip-time enet-uint32)
  (lowest-round-trip-time enet-uint32)
  (last-round-trip-time-variance enet-uint32)
  (highest-round-trip-time-variance enet-uint32)
  (round-trip-time enet-uint32)
  (round-trip-time-variance enet-uint32)
  (mtu enet-uint32)
  (window-size enet-uint32)
  (reliable-data-in-transit enet-uint32)
  (outgoing-reliable-sequence-number enet-uint32)
  (acknowledgements (:struct enet-list))
  (sent-reliable-commands (:struct enet-list))
  (sent-unreliable-commands (:struct enet-list))
  (outgoing-reliable-commands (:struct enet-list))
  (outgoing-unreliable-commands (:struct enet-list))
  (dispatched-commands (:struct enet-list))
  (needs-dispatch :int)
  (incoming-unsequenced-group enet-uint16)
  (outgoing-unsequenced-group enet-uint16)
  (unsequenced-window (:array enet-uint32
                              #.(floor +enet-peer-unsequenced-window-size+
                                       32)))
  (event-data enet-uint32)
  (total-waiting-data size-t)
  )

(defcstruct enet-compressor
  (context (:pointer :void))
  (compress :pointer)                   ; Callback function
  (decompress :pointer)                 ; Callback function
  (destroy :pointer)                    ; Callback function
  )

(defcstruct enet-host
  (socket enet-socket)
  (address (:struct enet-address))
  (incoming-bandwidth enet-uint32)
  (outgoing-bandwidth enet-uint32)
  (bandwidth-throttle-epoch enet-uint32)
  (mtu enet-uint32)
  (random-seed enet-uint32)
  (recalculate-bandwidth-limits :int)
  (peers (:pointer (:struct enet-peer)))
  (peer-count size-t)
  (channel-limit size-t)
  (service-time enet-uint32)
  (dispatch-queue (:struct enet-list))
  (continue-sending :int)
  (packet-size size-t)
  (header-flags enet-uint16)
  (commands (:array (:union enet-protocol)
                    #.+enet-protocol-maximum-packet-commands+))
  (command-count size-t)
  (buffers (:array (:struct enet-buffer)
                   #.+enet-buffer-maximum+))
  (buffer-count size-t)
  (checksum :pointer)                   ; Callback function
  (packet-data (:array (:array enet-uint8
                               #.+enet-protocol-maximum-mtu+)
                       2))
  (received-address (:struct enet-address))
  (received-data (:pointer enet-uint8))
  (received-data-length size-t)
  (total-sent-data enet-uint32)
  (total-received-data enet-uint32)
  (total-received-packets enet-uint32)
  (intercept :pointer)                  ; Callback function
  (connected-peers size-t)
  (bandwidth-limited-peers size-t)
  (duplicate-peers size-t)
  (maximum-packet-size size-t)
  (maximum-waiting-data size-t)
  )

(defcstruct enet-event
  (type enet-event-type)
  (peer (:pointer (:struct enet-peer)))
  (channel-id enet-uint8)
  (data enet-uint32)
  (packet (:pointer (:struct enet-packet)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcfun "enet_deinitialize" :void)
(defcfun "enet_initialize" :int)
;(defcfun "enet_initialize_with_callbacks" :int ...)
(defcfun "enet_linked_version" enet-version)

(defcfun "enet_host_create" (:pointer (:struct enet-host))
  (address (:pointer (:struct enet-address)))
  (peer-count size-t)
  (channel-limit size-t)
  (incoming-bandwidth enet-uint32)
  (outgoing-bandwidth enet-uint32))
(defcfun "enet_host_destroy" :void
  (host (:pointer (:struct enet-host))))
(defcfun "enet_host_service" :int
  (host (:pointer (:struct enet-host)))
  (event (:pointer (:struct enet-event)))
  (timeout enet-uint32))

(defcfun "enet_host_compress_with_range_coder" :int
  (host (:pointer (:struct enet-host))))

(defcfun "enet_peer_disconnect" :void
  (peer (:pointer (:struct enet-peer)))
  (data enet-uint32))

(defcfun "enet_peer_send" :void
  (peer (:pointer (:struct enet-peer)))
  (channel-id enet-uint8)
  (packet (:pointer (:struct enet-packet))))

(defcfun "enet_packet_create" (:pointer (:struct enet-packet))
  (data :pointer)
  (data-length size-t)
  (flags enet-uint32))

(defcfun "enet_packet_destroy" :void
  (packet (:pointer (:struct enet-packet))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
