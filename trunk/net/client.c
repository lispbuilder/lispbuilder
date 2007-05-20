/*
Sample echo client program.
*/

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include "net.h"
#include <assert.h>

#define BUF_SIZE 1024

#define CHECK(x) if ((x) < 0) { \
	printf(#x " failed, error: %s\n", strerror(errno)); \
	exit(1); \
}

void writeTest(int socket)
{
	// connected, send test data
	int sent = netWrite(socket, "test", 4);
	printf("sent: %i\n", sent);
	netSleep(1000);
}

void onRead(int socket)
{
	char buf[BUF_SIZE];
	int received;
	// server accepted incoming socket, echo data
	printf("onRead\n");
	received = netRead(socket, buf, BUF_SIZE);
	printf("received: %i\n", received);
	writeTest(socket);
}

void onWrite(int socket)
{
	printf("onWrite\n");
	writeTest(socket);
}

void onError(int socket)
{
	printf("onError: socket %i\n", socket);
	netCloseSocket(socket);
}

int main(int argc, char** argv)
{
	const char* address = "127.0.0.1";
	int port = 1234;
	int outgoingSocket;

	// register callbacks
	netRegisterReadCallback(onRead);
	netRegisterWriteCallback(onWrite);
	netRegisterErrorCallback(onError);

	// start network
	CHECK(netStartup());

	// create outgoing connection to the server socket
	CHECK(netCreateSocket(&outgoingSocket));

	// connect
	CHECK(netConnect(outgoingSocket, address, port));

	// process events
	while (1) {
		CHECK(netWait(1000));
		printf("tick\n");
	}

	// shutdown network
	CHECK(netCleanup());

	return 0;
}
