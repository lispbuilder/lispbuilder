#include "net-unix.h"
#include <sys/types.h>
#include <sys/event.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <sys/socket.h>  

static struct kevent* g_changelist = NULL;
static struct kevent* g_eventlist = NULL;
static int g_kqueueFd = 0;
static int numChangelistEvents = 0;

int netStartup()
{
	int result = netStartupUnix();
	if (result < 0) return result;
	g_kqueueFd = kqueue();
	if (g_kqueueFd < 0) return -NET_STARTUP_FAILED;
	g_changelist = malloc(sizeof(struct kevent) * g_maxFd);
	if (!g_changelist) return -NET_STARTUP_FAILED;
	g_eventlist = malloc(sizeof(struct kevent) * g_maxFd);
	if (!g_eventlist) return -NET_STARTUP_FAILED;
	memset(g_changelist, 0, sizeof(struct kevent) * g_maxFd);
	memset(g_eventlist, 0, sizeof(struct kevent) * g_maxFd);
	return 0;
}

int addNotifications(int fd)
{
	if (numChangelistEvents < g_maxFd) {
		fcntl(fd, F_SETFL, fcntl(fd, F_GETFL) | O_NONBLOCK);
		EV_SET(&g_changelist[numChangelistEvents++], fd, EVFILT_WRITE, EV_ADD | EV_ENABLE, 0, 0, 0);
		EV_SET(&g_changelist[numChangelistEvents++], fd, EVFILT_READ, EV_ADD | EV_ENABLE, 0, 0, 0);
		return 0;
	}
	return -1;
}

int disableWriteNotifcations(int fd)
{
	if (numChangelistEvents < g_maxFd) {
		EV_SET(&g_changelist[numChangelistEvents++], fd, EVFILT_WRITE, EV_ADD | EV_DISABLE, 0, 0, 0);
		return 0;
	}
	return -1;
}

int enableWriteNotifications(int fd)
{
	if (numChangelistEvents < g_maxFd) {
		EV_SET(&g_changelist[numChangelistEvents++], fd, EVFILT_WRITE, EV_ADD | EV_ENABLE, 0, 0, 0);
		return 0;
	}
	return -1;
}

int netCleanup()
{
	if (close(g_kqueueFd) == -1) return NET_CLEANUP_FAILED;
	free(g_eventlist);
	free(g_changelist);
	return netCleanupUnix();
}

int netWait(int millisecondsTimeout)
{
	int i;
	struct timespec timeout;
	timeout.tv_sec = millisecondsTimeout / 1000;
	timeout.tv_nsec = (millisecondsTimeout - timeout.tv_sec * 1000) * 1000000;

	int num = kevent(g_kqueueFd, g_changelist, numChangelistEvents, g_eventlist, g_maxFd, &timeout);
	if (num < 0) {
		return -NET_LOOP_FAILED;
	}
	numChangelistEvents = 0;

	if (num == 0) return 0;
	for (i = 0; i < num; i++) {
		struct kevent* pEvent = &g_eventlist[i];
		if (pEvent->flags & EV_ERROR) {
			printf("error: %s\n", strerror(pEvent->data));
			if (g_errorCallback) g_errorCallback(pEvent->ident);
			continue;
		}
		if (pEvent->filter == EVFILT_READ) {
			if (g_readCallback) g_readCallback(pEvent->ident);
		}
		if (pEvent->filter == EVFILT_WRITE) {
			disableWriteNotifcations(pEvent->ident);
			if (g_writeCallback) g_writeCallback(pEvent->ident);
		}
	}
	return 0;
}
