#include <stdio.h>
#include <stdlib.h>
#include "net.h"

SocketEventCallback g_readCallback = NULL;
SocketEventCallback g_writeCallback = NULL;
SocketEventCallback g_errorCallback = NULL;

void netRegisterReadCallback(SocketEventCallback callback)
{
	g_readCallback = callback;
}

void netRegisterWriteCallback(SocketEventCallback callback)
{
	g_writeCallback = callback;
}

void netRegisterErrorCallback(SocketEventCallback callback)
{
	g_errorCallback = callback;
}

void netFree(void* pointer)
{
	free(pointer);
}
