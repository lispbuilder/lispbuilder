#include "net-unix.h"
#include <sys/epoll.h>

static struct epoll_event* g_events;
static int g_epollFd;

int netStartup()
{
	int result = netStartupUnix();
	if (result < 0) return result;
	g_epollFd = epoll_create(g_maxFd);
	if (g_epollFd < 0) return -NET_STARTUP_FAILED;
	g_events = malloc(sizeof(struct epoll_event) * g_maxFd);
	if (!g_events) return -NET_STARTUP_FAILED;
	memset(g_events, 0, sizeof(struct epoll_event) * g_maxFd);
	return 0;
}

int addNotifications(int fd)
{
	if (fd < g_maxFd) {
		fcntl(fd, F_SETFL, fcntl(fd, F_GETFL) | O_NONBLOCK);
		g_events[fd].data.fd = fd;
		g_events[fd].events = EPOLLOUT | EPOLLIN;
		return epoll_ctl(g_epollFd, EPOLL_CTL_ADD, fd, &g_events[fd]);
	}
	return -1;
}

int disableWriteNotifications(int fd)
{
	if (fd < g_maxFd) {
		g_events[fd].events = EPOLLIN;
		return epoll_ctl(g_epollFd, EPOLL_CTL_MOD, fd, &g_events[fd]);
	}
	return -1;
}

int enableWriteNotifications(int fd)
{
	if (fd < g_maxFd) {
		g_events[fd].events = EPOLLIN | EPOLLOUT;
		return epoll_ctl(g_epollFd, EPOLL_CTL_MOD, fd, &g_events[fd]);
	}
	return -1;
}

int netCleanup()
{
	if (close(g_epollFd) == -1) return NET_CLEANUP_FAILED;
	free(g_events);
	return netCleanupUnix();
}

int netWait(int millisecondsTimeout)
{
	int i;
	int num = epoll_wait(g_epollFd, g_events, g_maxFd, millisecondsTimeout);
	if (num < 0) {
		return -NET_LOOP_FAILED;
	}
	if (num == 0) return 0;
	for (i = 0; i < num; i++) {
		struct epoll_event* pEvent = &g_events[i];
		if (pEvent->events & (EPOLLIN | EPOLLHUP)) {
			if (g_readCallback) g_readCallback(pEvent->data.fd);
		}
		if (pEvent->events & EPOLLOUT) {
			disableWriteNotifications(pEvent->data.fd);
			if (g_writeCallback) g_writeCallback(pEvent->data.fd);
		}
		if ((pEvent->events & EPOLLERR) && !(pEvent->events & EPOLLHUP)) {
			if (g_errorCallback) g_errorCallback(pEvent->data.fd);
		}
	}
	return 0;
}
