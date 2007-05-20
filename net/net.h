#ifndef NET_H
#define NET_H

#ifdef __cplusplus
extern "C" {
#endif

#ifdef _MSC_VER
#ifdef BUILD_DLL
#define NET_API __declspec(dllexport)
#else
#define NET_API __declspec(dllimport)
#endif
#else
#define NET_API
#endif

//! Possible error codes.
/*!
Every function, which returns an int, returns a value >= 0,
or a value < 0 for errors (use -NET_CONSTANT for the error value).
NET_WOULD_BLOCK is not an error, but an information.
*/
enum {
	NET_STARTUP_FAILED=1,  /*!< netStartup failed. */
	NET_CLEANUP_FAILED,  /*!< netCleanup failed. */
	NET_BIND_FAILED,  /*!< bind in netCreateServerSocket failed. */
	NET_CREATE_SOCKET_FAILED,  /*!< Socket creation in netCreateSocket or netCreateServerSocket failed. */
	NET_GET_HOST_BY_NAME_FAILED,  /*!< Host name lookup failed in netConnect or netCreateServerSocket. */
	NET_LISTEN_FAILED,  /*!< netListen failed. */
	NET_ACCEPT_FAILED,  /*!< netAccept failed. */
	NET_CLOSE_SOCKET_FAILED,  /*!< netCloseSocket failed. */
	NET_CONNECT_FAILED,  /*!< netConnect failed. */
	NET_WOULD_BLOCK,  /*!< netRead or netWrite result. */
	NET_WRITE_FAILED,  /*!< netWrite failed. */
	NET_READ_FAILED,  /*!< netRead failed. */
	NET_LOOP_FAILED,  /*!< netLoop failed. */
};

//! Signature of the callback functions.
typedef void(*SocketEventCallback)(int socket);

NET_API extern SocketEventCallback g_readCallback;
NET_API extern SocketEventCallback g_writeCallback;
NET_API extern SocketEventCallback g_errorCallback;

//! Register callback for read events.
/*!
The callback is called when data can be read without blocking or
when an incoming connection for a server socket can be accepted.
*/
NET_API void netRegisterReadCallback(SocketEventCallback callback);

//! Register callback for write events.
/*!
The callback is called when a socket was connected or
when it is possible to write more data with netWrite.
*/
NET_API void netRegisterWriteCallback(SocketEventCallback callback);

//! Register callback for write events.
/*!
The callback is called on error, e.g. netConnect timeout.
*/
NET_API void netRegisterErrorCallback(SocketEventCallback callback);

//! Initialize the network system.
/*!
*/
NET_API int netStartup();

//! Shutdown the network system.
/*!
*/
NET_API int netCleanup();

//! Create an outgoing socket.
/*!
*/
NET_API int netCreateSocket(int *socketHandle);

//! Create and bind a server socket (if address is NULL, bind to any address).
/*!
*/
NET_API int netCreateServerSocket(const char* address, int port, int *socketHandle);

//! Start listening on a server socket.
/*!
*/
NET_API int netListen(int serverSocket, int backlog);

//! Connect to the specified address and port.
/*!
*/
NET_API int netConnect(int socket, const char* address, int port);

//! Can be called on the accept callback to accept an incoming connection.
/*!
*/
NET_API int netAccept(int serverSocket, char** ipAddress, int *socketHandle);

//! Close a socket.
/*!
*/
NET_API int netCloseSocket(int socket);

//! Wait for an event or timeout.
/*!
*/
NET_API int netWait(int millisecondsTimeout);

//! Write to a socket.
/*!
There are three possible cases (result: returned value from this function) :
- result>=0: "result" number of bytes has been sent successfully.
- result=NET_WOULD_BLOCK and sent>0: Some data has been sent, more data can be sent when notified by the next write callback.
- result=NET_WRITE_FAILED: An error occured while writing to the socket, e.g. the socket is not connected.
*/
NET_API int netWrite(int socket, const void* data, int size);

//! Read from a socket.
/*!
There are four possible cases (result: returned value from this function) :
- result>0: "result" number of bytes has been read successfully.
- result=0: The connection has been closed.
- result=NET_WOULD_BLOCK: No data has been received, more data can be received when notified by the next read callback.
- result=NET_READ_FAILED: An error occured while reading from the socket.
*/
NET_API int netRead(int socket, void* data, int size);

//! Sleep for the specified time in milliseconds.
/*!
*/
NET_API void netSleep(int milliseconds);

//! Free memory, which was created by this library, e.g. the IP address in netAccept.
/*!
This function is neccessary, because memory allocated by this library could be incompatible
with the memory system used by programs which links this library.
*/
NET_API void netFree(void* pointer);

#ifdef __cplusplus
}
#endif

#endif
