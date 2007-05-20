#include <winsock2.h>
#include <windows.h>
#include <stdio.h>
#include "net.h"

#define WM_NET_SOCKET_NOTIFY (WM_USER + 1)

const char* windowClassName = "LispbuilderNetWindow";

HWND g_hwnd = 0;
int g_inLoop = 0;

LRESULT CALLBACK WndProc(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
	int error;
	SOCKET socket;

	switch (uMsg) {
		case WM_NET_SOCKET_NOTIFY:
			if (!g_inLoop) break;
			socket = (SOCKET)wParam;
			error = WSAGETSELECTERROR(lParam);
			switch (WSAGETSELECTEVENT(lParam)) {
				if (error == 0) {
					case FD_CLOSE:
						if (g_errorCallback) g_readCallback((int)socket);
						break;

					case FD_READ:
						if (g_readCallback) g_readCallback((int)socket);
						break;

					case FD_WRITE:
						if (g_writeCallback) g_writeCallback((int)socket);
						break;

					case FD_CONNECT:
						if (g_writeCallback) g_writeCallback((int)socket);
						break;

					case FD_ACCEPT:
						if (g_readCallback) g_readCallback((int)socket);
						break;
				} else {
					if (g_errorCallback) g_errorCallback((int)socket);
				}
			}
			break;

		default:
			return DefWindowProc( hWnd, uMsg, wParam, lParam );
	}
	return 0;
}

int netStartup()
{
	// init Winsock Library
	WORD wVersionRequested = MAKEWORD(1, 1);
	WSADATA wsaData;
	WNDCLASS wc;
	if (WSAStartup(wVersionRequested, &wsaData) != 0) return -NET_STARTUP_FAILED;

	// create message window
	ZeroMemory(&wc, sizeof(wc));
	wc.lpfnWndProc = (WNDPROC)WndProc;
	wc.cbClsExtra = 0;
	wc.cbWndExtra = 0;
	wc.hInstance = NULL;
	wc.lpszMenuName = NULL;
	wc.lpszClassName = windowClassName;
	RegisterClass(&wc);
	g_hwnd = CreateWindow(windowClassName, "", WS_OVERLAPPEDWINDOW, 0, 0, 0, 0, NULL, NULL, NULL, NULL);
	if (!g_hwnd) return -NET_STARTUP_FAILED;

	return 0;
}

int netCleanup()
{
	if (WSACleanup() != 0) return -NET_CLEANUP_FAILED;
	UnregisterClass(windowClassName, NULL);
	return 0;
}

int netCreateSocket(int* socketHandle)
{
	*socketHandle = (int) socket(AF_INET, SOCK_STREAM, 0); 
	if (*socketHandle == INVALID_SOCKET) return -NET_CREATE_SOCKET_FAILED;
	WSAAsyncSelect(*socketHandle, g_hwnd, WM_NET_SOCKET_NOTIFY, FD_READ | FD_WRITE | FD_CONNECT | FD_CLOSE);
	return 0;
}

int netCreateServerSocket(const char* address, int port, int* socketHandle)
{
	struct sockaddr_in local_addr; 
	int param = 1;

	// open new socket
	*socketHandle = (int) socket(AF_INET, SOCK_STREAM, 0);
	if (*socketHandle == INVALID_SOCKET) return -NET_CREATE_SOCKET_FAILED;

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
	WSAAsyncSelect(*socketHandle, g_hwnd, WM_NET_SOCKET_NOTIFY, FD_CLOSE | FD_ACCEPT);
	return 0;
}

int netListen(int serverSocket, int backlog)
{
	// listen for connections
	if (listen(serverSocket, backlog) < 0) return -NET_LISTEN_FAILED;
	return 0;
}

int netConnect(int socket, const char* address, int port)
{
	struct sockaddr_in serv_addr; 
	struct hostent *host;
	int result, error;
	serv_addr.sin_family = AF_INET; 
	serv_addr.sin_port = htons(port); 
	memset(&(serv_addr.sin_zero), 0, sizeof(serv_addr.sin_zero)); 
	if ((host = gethostbyname(address)) == NULL) {
		return -NET_GET_HOST_BY_NAME_FAILED;
	}
	serv_addr.sin_addr.s_addr = *((unsigned long *) host->h_addr);
	result = connect(socket, (struct sockaddr *) &serv_addr, sizeof(struct sockaddr));
	error = WSAGetLastError();
	if (result < 0) {
		if (WSAGetLastError() != WSAEWOULDBLOCK) return -NET_CONNECT_FAILED;
	}
	return 0;
}

int netAccept(int serverSocket, char** ipAddress, int* socketHandle)
{
	// accept connection
	struct sockaddr_in client_addr;
	int addrLen = sizeof(struct sockaddr_in);
	*socketHandle = (int) accept(serverSocket, ipAddress ? (struct sockaddr*) &client_addr : NULL, ipAddress ? &addrLen : NULL);
	if (*socketHandle == INVALID_SOCKET) return -NET_ACCEPT_FAILED;

	// get client address, if requested
	if (ipAddress) *ipAddress = strdup(inet_ntoa(client_addr.sin_addr));

	// register window notifications
	WSAAsyncSelect(*socketHandle, g_hwnd, WM_NET_SOCKET_NOTIFY, FD_READ | FD_WRITE | FD_CONNECT | FD_CLOSE);

	return 0;
}

int netCloseSocket(int socket)
{
	if (closesocket(socket) != 0) {
		return -NET_CLOSE_SOCKET_FAILED;
	}
	return 0;
}

int netWait(int millisecondsTimeout)
{
	BOOL bRet;
	MSG msg;
	UINT_PTR timerId = SetTimer(g_hwnd, 1, millisecondsTimeout, NULL);
	g_inLoop = 1;
	while ((bRet = GetMessage(&msg, NULL, 0, 0)) != 0) { 
		if (bRet == -1) {
			break;
		} else {
			if (msg.message == WM_TIMER) break;
			TranslateMessage(&msg); 
			DispatchMessage(&msg); 
		}
	}
	g_inLoop = 0;
	KillTimer(g_hwnd, timerId);
	return 0;
}

int netWrite(int socket, const void* data, int size)
{
	int result = result = send(socket, (char*) data, size, 0);
	if (result >= 0) {
		return result;
	} else {
		DWORD lastError = GetLastError();
		if (lastError == WSAEWOULDBLOCK) {
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
		DWORD lastError = GetLastError();
		if (lastError == WSAEWOULDBLOCK) {
			return -NET_WOULD_BLOCK;
		} else {
			return -NET_READ_FAILED;
		}
	}
}

void netSleep(int microseconds)
{
	Sleep(microseconds);
}
