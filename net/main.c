/*
Sample program without error and without partial read/write handling.
Expected output:

onConnect
sent: 4
onAccept
client ip: 127.0.0.1
onRead, incomingSocket
received: 4
sent: 4
onRead, outgoingSocket
received: 4

*/

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include "net.h"

// local server socket
int serverSocket;

// socket to server socket
int outgoingSocket;

// accepted incoming socket
int incomingSocket;

#define BUF_SIZE 1024

void onRead(int socket, int error)
{
	char buf[BUF_SIZE];
	if (socket == incomingSocket) {
		// server accepted incoming socket, echo data
		printf("onRead, incomingSocket\n");
		int received = 0;
		netRead(socket, buf, BUF_SIZE, &received);
		printf("received: %i\n", received);
		int sent = 0;
		netWrite(socket, buf, received, &sent);
		printf("sent: %i\n", sent);
	} else if (socket == outgoingSocket) {
		// read back echoed data
		printf("onRead, outgoingSocket\n");
		int received = 0;
		netRead(socket, buf, BUF_SIZE, &received);
		printf("received: %i\n", received);

		// stop loop
		netLoopFinish();
	}
}

void onConnect(int socket, int error)
{
	printf("onConnect\n");
	if (socket == outgoingSocket) {
		// connected, send test data
		int sent = 0;
		netWrite(socket, "test", 4, &sent);
		printf("sent: %i\n", sent);
	}
}

void onAccept(int socket, int error)
{
	// accept client connection
	printf("onAccept\n");
	char* ip;
	netAccept(socket, &incomingSocket, &ip);
	printf("client ip: %s\n", ip);
}

#define CHECK(x) if ((x) != NET_OK) { \
	printf(__STRING(x) " failed, error: %s\n", strerror(errno)); \
	exit(1); \
}

int main(int argc, char** argv)
{
	const char* address = "127.0.0.1";
	int port = 1234;

	// register callbacks
	netRegisterReadCallback(onRead);
	netRegisterConnectCallback(onConnect);
	netRegisterAcceptCallback(onAccept);

	// start network
	CHECK(netStartup());

	// create server socket
	CHECK(netCreateServerSocket(&serverSocket, NULL, port));

	// listen with 5 backlogs
	CHECK(netListen(serverSocket, 5));

	// create outgoing connection to the server socket
	CHECK(netCreateSocket(&outgoingSocket));

	// connect
	CHECK(netConnect(outgoingSocket, address, port));

	// process events
	CHECK(netLoop());

	// shutdown network
	CHECK(netCleanup());

	return 0;
}
