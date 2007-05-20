/*
Sample echo server program.
*/

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include "net.h"

// local server socket
int serverSocket;

#define BUF_SIZE 1024

#define CHECK(x) if ((x) < 0) { \
	printf(#x " failed, error: %s\n", strerror(errno)); \
	exit(1); \
}

void onRead(int socket)
{
	char buf[BUF_SIZE];
	if (socket == serverSocket) {
		// accept client connection
		char* ip;
		int incomingSocket;
		printf("onAccept\n");
		netAccept(socket, &ip, &incomingSocket);
		printf("client ip: %s\n", ip);
		netFree(ip);
	} else {
		// echo data
		int received, sent;
		printf("onRead\n");
		received = netRead(socket, buf, BUF_SIZE);
		printf("received: %i\n", received);
		if (received == 0) netCloseSocket(socket);
		sent = netWrite(socket, buf, received);
		printf("sent: %i\n", sent);
	}
}

void onWrite(int socket)
{
	printf("onWrite: socket: %i\n", socket);
}

void onError(int socket)
{
	printf("onError: socket %i\n", socket);
	netCloseSocket(socket);
}

int main(int argc, char** argv)
{
	int port = 1234;

	// register callbacks
	netRegisterReadCallback(onRead);
	netRegisterWriteCallback(onWrite);
	netRegisterErrorCallback(onError);

	// start network
	CHECK(netStartup());

	// create server socket
	CHECK(netCreateServerSocket(NULL, port, &serverSocket));

	// listen with 5 backlogs
	CHECK(netListen(serverSocket, 5));

	// process events
	while (1) {
		CHECK(netWait(1000));
		printf("tick\n");
	}

	// shutdown network
	CHECK(netCleanup());

	return 0;
}
