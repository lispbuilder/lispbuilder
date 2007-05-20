#include "net-unix.h"

int g_maxFd = 0;
sig_t g_oldSignal = NULL;

void sigpipe_handler(int i)
{
	// do nothing on SIGPIPE
}

int netStartupUnix()
{
	struct rlimit rlim;
	getrlimit(RLIMIT_NOFILE, &rlim);
	g_maxFd = rlim.rlim_cur;
	g_oldSignal = signal(SIGPIPE, sigpipe_handler);
	if (g_oldSignal == SIG_ERR) return -NET_STARTUP_FAILED;
	return 0;
}

int netCleanupUnix()
{
	sig_t s = signal(SIGPIPE, g_oldSignal);
	if (s == SIG_ERR) return -NET_CLEANUP_FAILED;
	return 0;
}

int netCreateSocket(int* socketHandle)
{
	*socketHandle = (int) socket(AF_INET, SOCK_STREAM, 0);
	if (*socketHandle == -1) return -NET_CREATE_SOCKET_FAILED;
	if (addNotifications(*socketHandle) == -1) return -NET_CREATE_SOCKET_FAILED;
	return 0;
}

int netCreateServerSocket(const char* address, int port, int* socketHandle)
{
	struct sockaddr_in local_addr; 
	int param = 1;

	// open new socket
	*socketHandle = (int) socket(AF_INET, SOCK_STREAM, 0);
	if (*socketHandle == -1) return -NET_CREATE_SOCKET_FAILED;

	// avoid EADDRINUSE error on bind()
	setsockopt(*socketHandle, SOL_SOCKET, SO_REUSEADDR, (char *)&param, sizeof(param));

	// bind the socket
	local_addr.sin_family = AF_INET; 
	local_addr.sin_port = htons(port); 
	memset(&(local_addr.sin_zero), 0, sizeof(local_addr.sin_zero)); 
	if (address) {
		struct hostent *host;
		if ((host = gethostbyname(address)) == NULL) {
			return -NET_GET_HOST_BY_NAME_FAILED;
		}
		local_addr.sin_addr.s_addr = *((unsigned long *) host->h_addr);
	} else {
		local_addr.sin_addr.s_addr = htonl(INADDR_ANY);
	}
	if (bind(*socketHandle, (struct sockaddr *) &local_addr, sizeof(struct sockaddr)) < 0) return -NET_BIND_FAILED;

	if (addNotifications(*socketHandle) == -1) return -NET_CREATE_SOCKET_FAILED;

	return 0;
}

int netListen(int serverSocket, int backlog)
{
	if (listen(serverSocket, backlog) < 0) return -NET_LISTEN_FAILED;
	return 0;
}

int netConnect(int socket, const char* address, int port)
{
	struct sockaddr_in serv_addr; 
	int error;
	serv_addr.sin_family = AF_INET; 
	serv_addr.sin_port = htons(port); 
	memset(&(serv_addr.sin_zero), 0, sizeof(serv_addr.sin_zero)); 
	struct hostent *host;
	if ((host = gethostbyname(address)) == NULL) {
		return -NET_GET_HOST_BY_NAME_FAILED;
	}
	serv_addr.sin_addr.s_addr = *((unsigned long *) host->h_addr);
	error = connect(socket, (struct sockaddr *) &serv_addr, sizeof(struct sockaddr));
	if (error < 0) {
		if (errno != EINPROGRESS) return -NET_CONNECT_FAILED;
	}
	return 0;
}

int netAccept(int serverSocket, char** ipAddress, int *socketHandle)
{
	// accept connection
	struct sockaddr_in client_addr;
	socklen_t addrLen = sizeof(struct sockaddr_in);
	*socketHandle = (int) accept(serverSocket, ipAddress ? (struct sockaddr*) &client_addr : NULL, ipAddress ? &addrLen : NULL);
	if (*socketHandle == -1) return -NET_ACCEPT_FAILED;

	// get client address, if requested
	if (ipAddress) *ipAddress = strdup(inet_ntoa(client_addr.sin_addr));

	if (addNotifications(*socketHandle) == -1) return -NET_ACCEPT_FAILED;

	return 0;
}

int netCloseSocket(int socket)
{
	if (close(socket) != 0) {
		return -NET_CLOSE_SOCKET_FAILED;
	}
	return 0;
}

int netWrite(int socket, const void* data, int size)
{
	int result = send(socket, (char*) data, size, 0);
	if (result >= 0) {
		return result;
	} else {
		if (errno == EAGAIN) {
			enableWriteNotifications(socket);
			return -NET_WOULD_BLOCK;
		} else {
			return -NET_WRITE_FAILED;
		}
	}
}

int netRead(int socket, void* data, int size)
{
	int result = recv(socket, (char*) data, size, 0);
	if (result >= 0) {
		return result;
	} else {
		if (errno == EINPROGRESS) {
			return -NET_WOULD_BLOCK;
		} else {
			return -NET_READ_FAILED;
		}
	}
}

void netSleep(int milliseconds)
{
	usleep(milliseconds * 1000);
}
