(in-package #:lispbuilder-net-cffi) 

(defconstant NET_STARTUP_FAILED -1)
(defconstant NET_CLEANUP_FAILED -2)
(defconstant NET_BIND_FAILED -3)
(defconstant NET_CREATE_SOCKET_FAILED -4)
(defconstant NET_GET_HOST_BY_NAME_FAILED -5)
(defconstant NET_LISTEN_FAILED -6)
(defconstant NET_ACCEPT_FAILED -7)
(defconstant NET_CLOSE_SOCKET_FAILED -8)
(defconstant NET_CONNECT_FAILED -9)
(defconstant NET_WOULD_BLOCK -10)
(defconstant NET_WRITE_FAILED -11)
(defconstant NET_READ_FAILED -12)
(defconstant NET_LOOP_FAILED -13)

(defcfun ("netRegisterReadCallback" netRegisterReadCallback) :void
  (callback :pointer))

(defcfun ("netRegisterWriteCallback" netRegisterWriteCallback) :void
  (callback :pointer))

(defcfun ("netRegisterErrorCallback" netRegisterErrorCallback) :void
  (callback :pointer))

(defcfun ("netStartup" netStartup) :int)

(defcfun ("netCleanup" netCleanup) :int)

(defcfun ("netCreateSocket" netCreateSocket) :int
  (socketHandle :pointer))

(defcfun ("netCreateServerSocket" netCreateServerSocket) :int
  (address :string)
  (port :int)
  (socketHandle :pointer))

(defcfun ("netListen" netListen) :int
  (serverSocket :int)
  (backlog :int))

(defcfun ("netConnect" netConnect) :int
  (socket :int)
  (address :string)
  (port :int))

(defcfun ("netAccept" netAccept) :int
  (serverSocket :int)
  (ipAddress :pointer)
  (socketHandle :pointer))

(defcfun ("netCloseSocket" netCloseSocket) :int
  (socket :int))

(defcfun ("netWait" netWait) :void
  (millisecondsTimeout :int))

(defcfun ("netWrite" netWrite) :int
  (socket :int)
  (data :pointer)
  (size :int))

(defcfun ("netRead" netRead) :int
  (socket :int)
  (data :pointer)
  (size :int))

(defcfun ("netSleep" netSleep) :void
  (milliseconds :int))

(defcfun ("netFree" netFree) :void
  (pointer :pointer))
