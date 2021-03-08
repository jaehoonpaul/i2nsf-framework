/*
 * Copyright 2006 Tail-f Systems AB
 */

/*
 * This program is an implementation of a subsystem for OpenSSH. It can
 * be used to use OpenSSH instead of the ConfD built-in ssh implementation.
 *
 * To use this, compile, and put the executable in /usr/local/bin
 *
 * Add the following line to your sshd_config file, and restart sshd.

Subsystem     netconf   /usr/local/bin/netconf-subsys

 * Make sure ConfD/NCS is configured to listen to TCP (and not SSH) on
 * localhost, port 2023 (can be changed in this code if necessary).
 * Alternatively, this program can be compiled to instead use a
 * connection to the ConfD/NCS IPC port (see below), in which case the
 * NETCONF server does not need to listen to TCP.
 *
 * Try to login using the normal ssh client:
 *   ssh -s myhost.com netconf
 *
 * You should get the hello message from the NETCONF agent.
 * Next, use netconf-console to send a query over ssh:
 *   netconf-console --host nyhost.com --port 22 -u myuser -p mypassword  \
 *     --get-config
 *
 * To compile this program to use a connection to the IPC port, USE_IPC
 * needs to be #define'd. In case the IPC Access Check is enabled in
 * ConfD/NCS, the environment variable $CONFD_IPC_ACCESS_FILE /
 * $NCS_IPC_ACCESS_FILE needs to be set to the full pathname of the file
 * containing the shared secret, just as for the API libraries. However
 * since it may be problematic to ensure that variables are set in this
 * program's environment, the pathname can instead be compiled in, via a
 * #define of IPC_ACCESS_FILE. Both #defines can be done either by
 * a) modifying the source here, with uncommented/adjusted lines:

#define USE_IPC
#define IPC_ACCESS_FILE "/path/to/access/file"

 * b) modifying the Makefile, or c) giving variable assignments on the
 * 'make' command line - see the Makefile for b) and c).
 */

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/tcp.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <sys/poll.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <ctype.h>
#include <stdarg.h>
#include <syslog.h>
#include <errno.h>
#include <pwd.h>
#include <grp.h>

#ifdef USE_IPC
#include "confd_ipc_access.h"
#endif

/* change this to match the value in confd.conf */
#define NETCONF_TCP_PORT 2023

#ifdef NCS
#define IPC_PORT 4569
#define IPC_ACCESS_FILE_ENV "NCS_IPC_ACCESS_FILE"
#else
#define IPC_PORT 4565
#define IPC_ACCESS_FILE_ENV "CONFD_IPC_ACCESS_FILE"
#endif
#define NETCONF_SOCK_ID 12
#define WANT_CHALLENGE (1 << 7)

static void fatal(int ecode, char *fmt, ...)
{
    va_list args, args2;

    if (strlen(fmt) > 0) {
        va_start(args, fmt);
        va_copy(args2, args);
        vsyslog(LOG_ERR, fmt, args);
        va_end(args);
        vfprintf(stderr, fmt, args2);
        va_end(args2);
        fprintf(stderr, "\n");
    }
    closelog();
    exit(ecode);
}

#ifdef USE_IPC
static void error(char *fmt, ...)
{
    va_list args;

    va_start(args,fmt);
    vsyslog(LOG_ERR, fmt, args);
    va_end(args);
}
#endif

static int write_fill(int fd, unsigned char *buf, int len)
{
    int i;
    unsigned int done = 0;

    do {
        if ((i = write(fd, (char *)(buf+done), len-done)) < 0) {
            if (errno != EINTR)
                return (i);
            i = 0;
        }
        done += i;
    } while (done < len);
    return (len);
}

static int read_write(int infd, int outfd)
{
    unsigned char buf[BUFSIZ];
    int rval;

    if ((rval = read(infd, buf, BUFSIZ)) == 0) {
        return 0;
    }
    if (rval < 0 && errno == EINTR)
        return 1;
    if (rval < 0)
        fatal(3, "Failed to read on fd %d: %d", infd, errno);
    if (write_fill(outfd, buf, rval) != rval)
        fatal(4, "Failed to write on fd %d: %d", outfd, errno);
    return 1;
}

#ifdef USE_IPC
static void do_connect(int sock)
{
    struct sockaddr_in addr;
    unsigned char secret[1024];
    int do_access;
    unsigned char socktype[1];

    addr.sin_addr.s_addr = inet_addr("127.0.0.1");
    addr.sin_family = AF_INET;
    addr.sin_port = htons(IPC_PORT);
    if (connect(sock,  (struct sockaddr*)&addr,
                sizeof (struct sockaddr_in)) < 0) {
        fatal(2, "Failed to connect to server");
    }

    /* get access check secret (if any) */
#ifdef IPC_ACCESS_FILE
    (void) setenv(IPC_ACCESS_FILE_ENV, IPC_ACCESS_FILE, 0);
#endif
    if ((do_access = confd_ipc_access_get_secret(secret, sizeof(secret))) < 0) {
        error("%s - skipping access check", secret);
        do_access = 0;
    }

    /* send socket type */
    socktype[0] = NETCONF_SOCK_ID;
    if (do_access)
        socktype[0] |= WANT_CHALLENGE;
    write_fill(sock, socktype, 1);

    /* run access check if needed */
    if (do_access && confd_ipc_access_check(sock, secret) != 1) {
        fatal(9, "Access check failed");
    }
    memset(secret, 0, sizeof(secret));
}
#else  /* netconf tcp */
static void do_connect(int sock)
{
    struct sockaddr_in addr;

    addr.sin_addr.s_addr = inet_addr("127.0.0.1");
    addr.sin_family = AF_INET;
    addr.sin_port = htons(NETCONF_TCP_PORT);

    if (connect(sock,  (struct sockaddr*)&addr,
                sizeof (struct sockaddr_in)) < 0)
        fatal(2, "Failed to connect to server");
}
#endif

static char *get_secondary_gids(gid_t gid, gid_t *groups, int ng)
{
    static char buf[BUFSIZ];
    int j;
    int len = 0;

    buf[0] = 0;
    for (j=0; j<ng; j++) {
        if (groups[j] != gid) {
            sprintf(&buf[len],"%d", groups[j]);
            len += strlen(&buf[len]);
            if (j != (ng-1)) {
                sprintf(&buf[len],",");
                len += 1;
            }
        }
    }
    return &buf[0];
}


/*
 * This function sends a header to ConfD, informing ConfD of the name of
 * the authenticated user, it's source IP (for logging purposes), and
 * optionally the name of the groups the user belong to.
 *
 * The format of the header is documented in the User's Guide:
 *  [<username>;<IP>;<proto>;<uid>;<gid>;<xtragids>;<homedir>;<group list>;]\n
 *
 * The uid, gid and homedir are used by ConfD when it starts external programs
 * to implement netconf extensions. These programs will run as uid/gid in the
 * homedir directory.  The xtragids are also used and a process executing on
 * behalf of a user will also have the xtragroups set
 * If extensions are not used, dummy values can be used
 * for uid/gid/homedir.
 * Note that the group list may be empty. The group list is used to check ConfD
 * authorization rules
 */
static void creds(int sock)
{
    char buf[BUFSIZ];
    char *ip, *port, *homedir, *user, *xtragids;
    char *ptr;
    gid_t gid;
    gid_t list[255];
    int n;

    if ((ptr = getenv("SSH_CONNECTION")) == NULL)
        fatal(5, "No SSH_CONNECTION environment variable found");
    if ((ip = strtok(ptr, " ")) == NULL)
        fatal(6, "Badly formatted SSH_CONNECTION environment variable");
    if ((port = strtok(NULL, " ")) == NULL)
        port = "0";
    homedir = getenv("HOME");
    user = getenv("USER");
    if (homedir == NULL)
        homedir = "/tmp";
    if (user == NULL)
        user = "";
    gid = getgid();
    n = getgroups(255, list);
    xtragids = get_secondary_gids(gid, list, n);
    sprintf(buf, "[%s;%s/%s;ssh;%d;%d;%s;%s;",
            user, ip, port,
            getuid(), gid,
            xtragids, homedir);
    /* extract group information */
    if (n > 0) {
        struct group *g;
        int i, len;
        int size = BUFSIZ - strlen(buf) - 3;

        for(i = 0; i < n && size > 0; i++) {
            g = getgrgid(list[i]);
            if (g != NULL) {
                len = strlen(g->gr_name);
                strncat(buf, g->gr_name, size);
                size -= len;
                if (i != n-1) {
                    strncat(buf, ",", size);
                    size--;
                }
            }
        }
    }
    strcat(buf, ";]\n");

    write(sock, buf, strlen(buf));
}

int main(int argc, char *argv[]) {
    int sock;
    struct pollfd fds[2];
    int nfds = 2;
    int one = 1;

    openlog("netconf-subsys", 0, LOG_DAEMON);
    if ((sock = socket(PF_INET, SOCK_STREAM, 0)) < 0 )
        fatal(1, "Failed to create socket");
    do_connect(sock);
    (void)setsockopt(sock, IPPROTO_TCP, TCP_NODELAY, &one, sizeof(one));

    /* Now we need to tell ConfD who we are */
    creds(sock);

    fds[0].fd = sock;
    fds[0].events = POLLIN;
    fds[1].fd = 0;
    fds[1].events = POLLIN;

    while (1) {
        fds[0].revents = 0;
        fds[1].revents = 0;

        if (poll(&fds[0], nfds, -1) < 0) {
            perror("Poll failed:");
            continue;
        }

        /* Check for I/O */
        if (fds[1].revents & (POLLIN | POLLHUP)) {
            if (read_write(0, sock) == 0) {
                /* eof on stdin - keep going w/o stdin */
                nfds = 1;
                shutdown(sock, SHUT_WR);
            }
        }
        else if (fds[1].revents) {
            fatal(7, "Poll error on stdin");
        }
        else if (fds[0].revents & (POLLIN | POLLHUP)) {
            if (read_write(sock, 1) == 0) {
                /* eof from server - we're done */
                exit(0);
            }
        }
        else if (fds[0].revents) {
            fatal(8, "Poll error from server");
        }
    }
}
