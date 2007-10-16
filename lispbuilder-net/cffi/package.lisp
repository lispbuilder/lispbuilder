;;;; lispbuilder-net
;;;; (C) 2007 Frank Buss <fb@frank-buss.de>

(in-package #:cl-user)

(defpackage #:lispbuilder-net-cffi
  (:use #:cl #:cffi)
  (:nicknames #:net-cffi)
  (:documentation "The basic wrapper package of `lispbuilder-net'.")
  
  (:export
   #:*default-surface*
   #:*default-display*
   #:NET_STARTUP_FAILED
   #:NET_CLEANUP_FAILED
   #:NET_BIND_FAILED
   #:NET_CREATE_SOCKET_FAILED
   #:NET_GET_HOST_BY_NAME_FAILED
   #:NET_LISTEN_FAILED
   #:NET_ACCEPT_FAILED
   #:NET_CLOSE_SOCKET_FAILED
   #:NET_CONNECT_FAILED
   #:NET_WOULD_BLOCK
   #:NET_WRITE_FAILED
   #:NET_READ_FAILED
   #:NET_LOOP_FAILED
   #:NET_STARTUP_FAILED
   #:NET_STARTUP_FAILED
   #:NET_STARTUP_FAILED
   #:NET_STARTUP_FAILED
   #:NET_STARTUP_FAILED
   #:NET_STARTUP_FAILED
   #:NET_STARTUP_FAILED
   #:netRegisterReadCallback
   #:netRegisterWriteCallback
   #:netRegisterErrorCallback
   #:netStartup
   #:netCleanup
   #:netCreateSocket
   #:netCreateServerSocket
   #:netListen
   #:netConnect
   #:netAccept
   #:netCloseSocket
   #:netWait
   #:netWrite
   #:netRead
   #:netSleep
   #:netFree))
