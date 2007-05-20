#ifndef NET_UNIX_H
#define NET_UNIX_H

#include <sys/types.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <sys/socket.h>  
#include <netinet/in.h>
#include <arpa/inet.h>
#include <errno.h>
#include <unistd.h>
#include <fcntl.h>
#include <string.h>
#include <netdb.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include "net.h"

extern int g_maxFd;
extern sig_t g_oldSignal;

// implemented by the specific Unix implementation
int addNotifications(int fd);
int disableWriteNotifications(int fd);
int enableWriteNotifications(int fd);

int netStartupUnix();
int netCleanupUnix();
int netCreateSocketUnix(int* socketHandle);
int netCreateServerSocketUnix(const char* address, int port, int* socketHandle);
int netAcceptUnix(int serverSocket, char** ipAddress, int *socketHandle);

#endif
