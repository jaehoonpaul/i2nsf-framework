/*    -*- C -*-
 *
 *  Copyright 2006 Tail-F Systems AB. All rights reserved.
 *
 *  This software is the confidential and proprietary
 *  information of Tail-F Systems AB.
 *
 *    File:      clistart.c
 *    Author:    Johan Bevemyr
 *    Created:   Thu Jan 19 02:08:29 2006
 *
 *  Compile with
 *
 *     gcc -o confd_cli clistart.c -lcurses
 *
 *  Possible exit codes:
 *    0 - normal exit
 *    1 - failed to read user data for initial handshake
 *    2 - close timeout, client side closed, session inactive
 *    3 - idle timeout triggered
 *    4 - tcp level error detected on ConfD side
 *    5 - internal error occured in ConfD/NCS
 *    6 - user interrupted clistart using special escape char (only
 *        generated locally in clistart/confd_cli/ncs_cli
 *    7 - ConfD/NCS abruptly closed socket (generated locally in C program)
 *    8 - ConfD/NCS stopped on error
 */

#define CLISTART_PROTO_VSN "1.0"

#include <pwd.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/poll.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <string.h>
#include <getopt.h>
#include <sys/types.h>
#include <grp.h>
#include <signal.h>
#include <errno.h>
#include <fcntl.h>
#include <ctype.h>
#include <limits.h>

#ifdef NCURSES
#include <curses.h>
#else
#include <termios.h>
#include <sys/ioctl.h>
#endif

#include "confd_ipc_access.h"

#ifdef NCS
#define SERVER "NCS"
#define PORT "4569"
#define CLI_OPAQUE "NCS_CLI_OPAQUE"
#define IPC_ADDR "NCS_IPC_ADDR"
#define IPC_PORT "NCS_IPC_PORT"
#define IPC_EXTADDR "NCS_IPC_EXTADDR"
#define IPC_EXTSOPATH "NCS_IPC_EXTSOPATH"
#else
#define SERVER "ConfD"
#define PORT "4565"
#define CLI_OPAQUE "CONFD_CLI_OPAQUE"
#define IPC_ADDR "CONFD_IPC_ADDR"
#define IPC_PORT "CONFD_IPC_PORT"
#define IPC_EXTADDR "CONFD_IPC_EXTADDR"
#define IPC_EXTSOPATH "CONFD_IPC_EXTSOPATH"
#endif
#define CLI_SOCK_ID 4
#define WANT_CHALLENGE (1 << 7)
#define IA_PROTO_UNAVAILABLE 13

/* Inband signalling codes */
#define INBAND_SIGWINCH  1
#define INBAND_WRITE_ACC 2


#define USER_ARGS /* Comment this out to remove the possibility to send
                   * a user provided user name. When used in a production
                   * environment you may want to remove this option and
                   * possibly some more, to prevent users from masquerading
                   * as other users.
                   */

#define FULL_ACCESS /* Comment this out to disable the ability for the
                     * confd_cli/ncs_cli to login as nouser atall but with all
                     * authorization turned off. The feature can be used
                     * to login to the CLI when the box is broken. For
                     * example when the AAA data is broken.
                     */

/* #define EXTERNAL_IPC */ /* Uncomment this to provide support for user-
                            * defined IPC towards the ConfD/NCS daemon.
                            * The CONFD_IPC_EXTADDR and CONFD_IPC_EXTSOPATH
                            * - or NCS_IPC_EXTADDR and NCS_IPC_EXTSOPATH -
                            * environment variables can then be used to
                            * request a connection using this IPC mechanism,
                            * see the deployment chapter in the User Guide.
                            * Note, on Linux this requires that -ldl is
                            * added to the LIBS definition in the Makefile.
                            */

#ifdef EXTERNAL_IPC
#include <dlfcn.h>
#include "ipc_drv.h"
#endif

#define put_int32(i, s) {((char*)(s))[0] = (char)((i) >> 24) & 0xff; \
                         ((char*)(s))[1] = (char)((i) >> 16) & 0xff; \
                         ((char*)(s))[2] = (char)((i) >> 8)  & 0xff; \
                         ((char*)(s))[3] = (char)((i)        & 0xff);}

#define get_int32(i, s) {i = (((((char*)(s))[0] & 0xff) << 24) |        \
                              ((((char*)(s))[1] & 0xff) << 16) |        \
                              ((((char*)(s))[2] & 0xff) <<  8) |        \
                              ((((char*)(s))[3] & 0xff)         ));}

static int interactive;
static int verbose = 0;
static int read_term_sz = 0; /* flag set by SIGWINCH handler */
static int width, height;
static int noaaa = 0;
static int stop_on_error = 0;
static char* nl_interactive = "\r\n";
static char* nl_batch       = "\n";
static char* nl;
static int nl_len = 1;
#ifdef __APPLE__
static int old_raw = 1;
#else
static int old_raw = 0;
#endif
struct pollfd fds[3];

static char *block0;
static int  block0_remain;
static char *block1;
static int  block1_remain;
static char in0buf[1024];
static char in1buf[1024];
static char out0buf[2048];
static char out1buf[2048];

static unsigned int write_count = 0;
static int write_count_pending = 0;
static char pendbuf[6];


static int write_fill_confd_noesc(int fd, char *buf, int len)
{
    int i;
    unsigned int done = 0;

    do {
        if ((i = write(fd, (buf+done), len-done)) < 0) {
            if (errno == EAGAIN || errno == EWOULDBLOCK) {
                block0 = (buf+done);
                block0_remain = len-done;
                /* wait until ConfD is ready for more data */
                fds[1].events = fds[1].events | POLLOUT;
                /* plock additional input from terminal */
                fds[0].events = fds[0].events & ~POLLIN;
                return 0;
            }
            if (errno != EINTR)
                return (i);
            i = 0;
        }
        done += i;
    } while (done < len);

    /* check if we have a pend buf and write that if we can */
    if (write_count_pending == 1) {
        write_count_pending = 0;

        int i;
        for(i=0 ; i < 6 ; i++)
            out0buf[i] = pendbuf[i];

        write_fill_confd_noesc(fd, out0buf, 6);
    }

    return (len);
}

static int write_fill_confd(int fd, char *buf, int len)
{
    int i, n, ret;

    for(i=0, n=0 ; i < len ; i++, n++) {
        if (buf[i] == 0) {
            out0buf[n++] = 0;
        }
        out0buf[n] = buf[i];
    }

    ret = write_fill_confd_noesc(fd, out0buf, n);

    return ret;
}

static int write_fill_term(int fd, char *buf, int len)
{
    int i;
    unsigned int done = 0;

    do {
        if ((i = write(fd, (buf+done), len-done)) < 0) {
            if (errno == EAGAIN || errno == EWOULDBLOCK) {
                block1 = (buf+done);
                block1_remain = len-done;
                /* wait until term is ready for more data */
                fds[2].events = fds[2].events | POLLOUT;
                /* plock additional input from confd */
                fds[1].events = fds[1].events & ~POLLIN;
                return 0;
            }
            if (errno != EINTR)
                return (i);
            i = 0;
        }
        write_count += i;
        done += i;
    } while (done < len);

    /* time to report? */
    if (!interactive) {
        /* not interactive, ignore accs */
        write_count = 0;
    }
    else if (write_count > 1024) {
        /* can we write directly? */
        if (!(fds[1].events & POLLOUT)) {
            put_int32(write_count, &out0buf[2]);
            out0buf[0] = 0;  /* ESC char */
            out0buf[1] = INBAND_WRITE_ACC;
            write_fill_confd_noesc(fds[1].fd,  out0buf, 6);

            write_count = 0;
        }
        else if (write_count_pending == 0) {
            write_count_pending = 1;
            put_int32(write_count, &pendbuf[2]);
            pendbuf[0] = 0;  /* ESC char */
            pendbuf[1] = INBAND_WRITE_ACC;
            write_count = 0;
        }
        else {
            // update existing write count
            int oldcount;
            get_int32(oldcount, &pendbuf[2]);
            write_count += oldcount;
            put_int32(write_count, &pendbuf[2]);
            write_count = 0;
        }
    }

    return (len);
}


static int write_int(int fd, int val)
{
    char buf[4];
    put_int32(val, &buf[0]);
    return write_fill_confd(fd, buf, 4);
}


/* Connect to the ConfD/NCS daemon */
static int cli_connect(char *address, char *port)
{
    struct addrinfo hints;
    struct addrinfo *addr;
    int fd;

    memset(&hints, 0, sizeof(hints));
    hints.ai_family = PF_UNSPEC;
    hints.ai_socktype = SOCK_STREAM;
    hints.ai_flags = AI_NUMERICHOST;

    if (getaddrinfo(address, port, &hints, &addr) != 0) return -1;

    fd = socket(addr->ai_family, addr->ai_socktype, addr->ai_protocol);
    if (fd < 0) return -1;

    if (connect(fd, addr->ai_addr, addr->ai_addrlen) < 0)
        return -1;

    freeaddrinfo(addr);

    return fd;
}

#ifdef EXTERNAL_IPC
/* Connect to the ConfD/NCS daemon using user-provided IPC */
static int ext_cli_connect(char *addr)
{
    char *path;
    void *handle;
    confd_ext_ipc_init_func_t *ext_init_ops;
    struct confd_ext_ipc_cbs *ecb = NULL;
    confd_ipc_init_func_t *init_ops;
    struct confd_ipc_cbs *cb = NULL;
    char *errstr;
    int fd;

    if ((path = getenv(IPC_EXTSOPATH)) == NULL) {
        fprintf(stderr, "Environment " IPC_EXTSOPATH " must "
                "be set when using external IPC\n");
        return -1;
    }
    if ((handle = dlopen(path, RTLD_LOCAL|RTLD_LAZY)) == NULL) {
        fprintf(stderr, "Failed to load %s\n", path);
        return -1;
    }
    if ((ext_init_ops = (confd_ext_ipc_init_func_t *)
         dlsym(handle, "confd_ext_ipc_init")) != NULL) {
        ecb = (*ext_init_ops)();
    } else if ((init_ops = (confd_ipc_init_func_t *)
                dlsym(handle, "confd_ipc_init")) != NULL) {
        cb = (*init_ops)();
    }

    if (ecb != NULL) {
        if (ecb->connect != NULL) {
            if ((fd = ecb->connect(addr, &errstr)) < 0)
                return -1;
        } else {
            int family, type, protocol;
            struct sockaddr *saddr;
            socklen_t addrlen;
            if (ecb->getaddrinfo(addr, &family, &type, &protocol,
                                 &saddr, &addrlen, &errstr) < 0)
                return -1;
            if ((fd = ecb->socket(family, type, protocol, &errstr)) < 0)
                return -1;
            if (connect(fd, saddr, addrlen) < 0)
                return -1;
            free(saddr);
        }
    } else if (cb != NULL) {
        if ((fd = cb->connect(addr)) < 0)
            return -1;
    } else {
        fprintf(stderr, "Failed to init %s\n", path);
        return -1;
    }

    if (verbose)
        fprintf(stderr, "Connected to CLI server\n");

    return fd;
}
#endif

#ifndef NCURSES
struct termios prev_state;

static int tty_raw(int fd)
{
    struct termios  b;
    unsigned int iflag, lflag, cflag, oflag;

    if (tcgetattr(fd, &prev_state) < 0) return -1;

    iflag = prev_state.c_iflag;
    lflag = prev_state.c_lflag;
    cflag = prev_state.c_cflag;
    oflag = prev_state.c_oflag;

    b = prev_state;

    if (old_raw) {
        iflag = iflag & ~(ISTRIP | IXON | BRKINT | ICRNL);
        lflag = lflag & ~(ECHO | ICANON | IEXTEN | ISIG);
        cflag = (cflag & ~(CSIZE | PARENB)) | CS8;
        oflag = oflag & ~OPOST;
    }
    else {
        iflag = iflag & ~(ISTRIP | IXON | BRKINT | ICRNL);
        lflag = lflag & ~(ECHO | ICANON | IEXTEN | ISIG);
        cflag = (cflag & ~(CSIZE | PARENB)) | CS8;
        oflag = oflag | OPOST | ONLCR;
    }

    b.c_iflag = iflag;
    b.c_lflag = lflag;
    b.c_cflag = cflag;
    b.c_oflag = oflag;

    b.c_cc[VMIN] = 1;
    b.c_cc[VTIME] = 0;

    if (tcsetattr(fd, TCSAFLUSH, &b) < 0) return -1;

    return 0;
}

static int tty_restore(int fd)
{
    if (tcsetattr(fd, TCSAFLUSH, &prev_state) < 0)
        return -1;

    return 0;
}
#endif

int prev_fd_flags, prev_outfd_flags;

void set_nonblocking(int fd, int outfd)
{
    /* configure socket for non-blocking io */
    prev_fd_flags = fcntl(fd, F_GETFL, 0);
    fcntl(fd, F_SETFL, prev_fd_flags | O_NONBLOCK);

    prev_outfd_flags = fcntl(outfd, F_GETFL, 0);
    fcntl(outfd, F_SETFL, prev_outfd_flags | O_NONBLOCK);
}

void restore_blocking(int fd, int outfd)
{
    /* restore blocking io */
    fcntl(fd, F_SETFL, prev_fd_flags);
    fcntl(outfd, F_SETFL, prev_outfd_flags);
}

static void usage(char *cmd) {
    fprintf(stderr, "Usage: %s [options] [file]\n", cmd);
    fprintf(stderr, "Options:\n");
    fprintf(stderr, "  --help, -h            display this help\n");
    fprintf(stderr, "  --host, -H <host>     current host name "
            "(used in prompt)\n");
    fprintf(stderr, "  --address, -A <addr>  cli address to connect to\n");
    fprintf(stderr, "  --port, -P <port>     cli port to connect to\n");
    fprintf(stderr, "  --cwd,  -c <dir>      current working directory\n");
    fprintf(stderr, "  --proto, -p <proto>   type of connection (tcp, ssh, "
            "console)\n");
    fprintf(stderr, "  --verbose, -v         verbose output\n");
    fprintf(stderr, "  --ip, -i              clients source ip[/port]\n");
    fprintf(stderr, "  --interactive, -n     force interactive mode\n");
    fprintf(stderr, "  --escape-char, -E <C> brute force shutdown when user "
            "enters ASCII C\n");
#ifndef NCS
    fprintf(stderr, "  --old-raw, -o         use raw tty processing for tty "
            "sessions\n");
    fprintf(stderr, "  --noninteractive, -N  force noninteractive mode\n");
    fprintf(stderr, "  --ttyname, -T <name>  tty name\n");
    fprintf(stderr, "  --terminal, -t <name> terminal name\n");
#endif
    fprintf(stderr, "  -J                    Juniper style CLI\n");
    fprintf(stderr, "  -C                    Cisco XR style CLI\n");
#ifndef NCS
    fprintf(stderr, "  -I                    Cisco IOS style CLI\n");
#endif
#ifdef USER_ARGS
    fprintf(stderr, "  --user, -u <user>     clients user name\n");
    fprintf(stderr, "  --uid, -U <uid>       clients user id\n");
    fprintf(stderr, "  --groups, -g <groups> clients group list\n");
    fprintf(stderr, "  --gids, -D <gids>     clients group id list\n");
    fprintf(stderr, "  --gid, -G <gid>       clients group id\n");
#endif
#ifdef FULL_ACCESS
    fprintf(stderr, "  --noaaa               disable AAA\n");
#endif
    fprintf(stderr, "  --opaque, -O <opaque> pass opaque info\n");
    fprintf(stderr, "  --stop-on-error, -s   stop on error\n");
    fprintf(stderr, "\n");
}

static struct option long_options[] = {
    {"host",    1, 0, 'H'},
    {"cwd",     1, 0, 'c'},
    {"proto",   1, 0, 'p'},
    {"address", 1, 0, 'A'},
    {"port",    1, 0, 'P'},
    {"help",    0, 0, 'h'},
    {"verbose", 0, 0, 'v'},
    {"ip",      1, 0, 'i'},
    {"interactive",      0, 0, 'n'},
    {"noninteractive",      0, 0, 'N'},
    {"old-raw",      0, 0, 'o'},
    {"cisco",   0, 0, 'C'},
    {"juniper", 0, 0, 'J'},
#ifndef NCS
    {"ios",     0, 0, 'I'},
#endif
    {"ttyname", 1, 0, 'T'},
#ifdef USER_ARGS
    {"user",    1, 0, 'u'},
    {"uid",     1, 0, 'U'},
    {"groups",  1, 0, 'g'},
    {"gids",    1, 0, 'D'},
    {"gid",     1, 0, 'G'},
#endif
#ifdef FULL_ACCESS
    {"noaaa",  0, 0, 'a'},
#endif
    {"terminal",1, 0, 't'},
    {"opaque",  1, 0, 'O'},
    {"escape-char",  1, 0, 'E'},
    {"stop-on-error",  0, 0, 's'},
    {0,         0, 0, 0}
};


static void sig_handler(int sig)
{
    if (sig == SIGWINCH)
        read_term_sz = 1;
    return;
}

static void get_size()
{
#ifdef NCURSES
    static WINDOW *w;
    getmaxyx(w, height, width);
#else
    {
        struct winsize size;
        ioctl(STDIN_FILENO, TIOCGWINSZ, (char *) &size);

        width = size.ws_col;
        height = size.ws_row;

    }
#endif
}

int is_ip_char(char c)
{
    return
        isdigit(c) ||
        c == '.' ||
        c == ':' ||
        (c >= 'a' && c <= 'f') ||
        (c >= 'A' && c <= 'F');
}


int main(int argc, char *argv[])
{
    struct pollfd *fdsptr = fds;
    struct passwd *pwd = NULL;
    int nfds;
    int fd;
    char *user = NULL;
    char *term = NULL;
    char ip[1024];
    char ctype[1024];
    char *host = NULL;
    char tty_name[1024];
    char socktype[1];
    char cwd[1024];
    char *groups = NULL;
    int *gids = NULL;
    int gidsn = 0;
    int uid;
    int gid;
    char *opaque = NULL;
    int interactive_send = -1;
    char *address = "127.0.0.1";
    char *port = PORT;
    char *ssh_connection;
    int escape_count=0;
    int infd = fileno(stdin);
    int outfd = fileno(stdout);
    char *style = "unset";
#ifdef EXTERNAL_IPC
    char *extaddr;
#endif
    unsigned char secret[1024];
    int do_access;
    int exit_code=7;
    int next_may_be_exit_code=0;
    char escape=-1;
    int num_read;
    char control_ch;

    /* Check if we are invoked from an OpenSSH connection */
    ssh_connection = getenv("SSH_CONNECTION");
    if (ssh_connection) {
        char *end=NULL;

        strcpy(ctype, "ssh");
        /* look for first two elements of ssh_connection */
        end = strchr(ssh_connection, ' ');
        if (end != NULL) {
            int i;
            int n = end-ssh_connection;
            char *p = end;

            for(i=0 ; i < n && is_ip_char(ssh_connection[i]) ; i++)
                ip[i] = ssh_connection[i];

            ip[n] = '\0';

            /* Skip any trailing stuff in the ip address, for example,
             * zone index (%eth).
             */
            while(*p != ' ' && *p != '\0')
                p++;

            /* skip whitespace */
            while (*p == ' ')
                p++;

            end = strchr(p, ' ');
            if (end != NULL) {
                n = end - p;
                sprintf(&ip[strlen(ip)], "/%.*s", n, p); /* address/port */
            }
        }
        else {
            strcpy(ip, "127.0.0.1");
        }
    }
    else {
        /* default to console as we do not detect telnet at this point */
        strcpy(ctype, "console");
        strcpy(ip, "127.0.0.1");
    }

    if (getcwd(cwd, sizeof(cwd)) == NULL) {
        perror("getcwd");
        return 1;
    }

    /* Check environment for address/port to connect to the  daemon */
    {
        char *atmp = getenv(IPC_ADDR);
        char *ptmp = getenv(IPC_PORT);
        if (atmp) {
            address = atmp;
        }
        if (ptmp) {
            port = ptmp;
        }
    }

    /* we use the effective uid of current process */
    uid = geteuid();

    /* we use the effective gid of current process */
    gid = getegid();

    /* determine default ttyname */
    {
        char *tmp = ttyname(infd);

        tty_name[sizeof(tty_name)-1] = '\0';

        if (tmp) {
            strncpy(tty_name, tmp, sizeof(tty_name)-1);
        }
        else {
            tty_name[0] = '\0';
        }
    }

    /* check environment for "opaque" string */
    opaque = getenv(CLI_OPAQUE);

    /* Process command line arguments */
    while(1) {
        int option_index;
        int c;

        /* this call can easily be replaced with a more portable version
         * if needed
         */
        c = getopt_long(argc, argv, "c:hp:H:A:P:vi:u:U:g:G:D:t:nNaCIJE:T:oO:s",
                        long_options, &option_index);

        if (c == -1) break;

        switch(c) {
        case 'H':
            host = optarg;
            break;

        case 'T':
            tty_name[sizeof(tty_name)-1] = '\0';
            strncpy(tty_name, optarg, sizeof(tty_name)-1);
            break;

        case 'c':
            cwd[sizeof(cwd)-1] = '\0';
            strncpy(cwd, optarg, sizeof(cwd)-1);
            break;

        case 'h':
            usage(argv[0]);
            return 0;

        case 'p':
            ctype[sizeof(ctype)-1] = '\0';
            if (strcmp(optarg, "tcp") != 0 &&
                strcmp(optarg, "ssh") != 0 &&
                strcmp(optarg, "http") != 0 &&
                strcmp(optarg, "https") != 0 &&
                strcmp(optarg, "console") != 0) {
                fprintf(stderr, "Error: unsupported protocol type: %s\n",
                        optarg);
                usage(argv[0]);
                exit(1);
            }
            else
                strncpy(ctype, optarg, sizeof(ctype)-1);
            break;

        case 'A':
            address = optarg;
            break;

        case 'o':
            old_raw = 1;
            break;

        case 'P':
            port = optarg;
            break;

        case 'v':
            verbose = 1;
            break;

        case 'i':
            ip[sizeof(ip)-1] = '\0';
            strncpy(ip, optarg, sizeof(ip)-1);
            break;

        case 'n':
            interactive_send = 1;
            break;
        case 'N':
            interactive_send = 0;
            break;
#ifndef NCS
        case 'I':
            style = "i";
            break;
#endif
        case 'J':
            style = "j";
            break;

        case 'C':
            style = "c";
            break;
#ifdef USER_ARGS
        case 'u':
            user = optarg;
            break;

        case 'U':
            uid = atoi(optarg);
            break;

        case 'g':
            groups = optarg;
            break;

        case 'D':
        {
            /* parse lists of gids, for example
             * 10,201,45
             */

            /* count upper bound on groups first */
            int i=1;
            char *gidsstr = optarg;
            char *tmp = gidsstr;

            while(*tmp != '\0') {
                if (*tmp == ',') i++;
                tmp++;
            }

            gids = (int *) malloc(sizeof(int)*i);

            if (gids == NULL) {
                fprintf(stderr, "Failed to allocate memory.");
                exit(1);
            }

            i = 0;
            while((tmp = strtok(gidsstr, ",")) != NULL) {
                char *endptr;

                gidsstr = NULL;

                gids[i++] = (int) strtol(tmp, &endptr, 0);
                if (endptr == tmp) {
                    /* no digit found */
                    fprintf(stderr, "Error: illegal gid %s\n", tmp);
                    exit(1);
                }
            }

            gidsn = i;

            break;
        }

        case 'G':
            gid = atoi(optarg);
            break;
#endif
#ifdef FULL_ACCESS
        case 'a':
            noaaa = 1;
            break;
#endif
        case 't':
            term = optarg;
            break;

        case 'O':
            opaque = optarg;
            break;

        case 'E':
            escape = (char) atoi(optarg);
            break;

        case 's':
            stop_on_error = 1;
            break;

        default:
            usage(argv[0]);
            return 1;
        }
    }

    if (optind < argc) {
        /* a file argument was supplied, read input from file */
        int filefd = open(argv[optind], O_RDONLY);

        if (filefd > 0) {
            infd = filefd;
        }
        else {
            fprintf(stderr, "%s: failed to open %s\n", argv[0], argv[optind]);
            exit(1);
        }
    }

#ifdef EXTERNAL_IPC
    if ((extaddr = getenv(IPC_EXTADDR)) != NULL)
        fd = ext_cli_connect(extaddr);
    else
#endif
        fd = cli_connect(address, port);

    if (fd < 0) {
        fprintf(stderr, "Failed to connect to server\n");
        return 1;
    }

    /* get access check secret */
    if ((do_access = confd_ipc_access_get_secret(secret, sizeof(secret))) < 0) {
        fprintf(stderr, "%s\n", secret);
        do_access = 0;
    }

    /* send socket type */
    socktype[0] = CLI_SOCK_ID;
    if (do_access)
        socktype[0] |= WANT_CHALLENGE;
    write_fill_confd(fd,  socktype, 1);

    /* Check that ConfD really accepts the CLI
     * Read byte from ConfD determining if CLI is registered
     */
    if ((num_read = read(fd, &control_ch, 1)) <= 0) {
        if (num_read == 0 || (num_read < 0 && errno == ECONNRESET)) {
            num_read = -2;
        }
        if (errno != EINTR) {
            num_read = -1;
        }
    }

    /* If ConfD is in phase1, CLI is not allowed to connect
     * If socket is down but access check is required, skip this
     */
    if((num_read == 1 && control_ch == IA_PROTO_UNAVAILABLE) ||
       (num_read < 0 && do_access == 0)) {
        fprintf(stderr, "Failed to connect to server\n");
        return 1;
    }

    /* run access check if needed */
    if (do_access && confd_ipc_access_check(fd, secret) != 1) {
        fprintf(stderr, "Access check failed\n");
        return 1;
    }
    memset(secret, 0, sizeof(secret));

    /* determine user name */
    if (user == NULL) {
      user = getlogin();
      if (user == NULL) user = getenv("LOGNAME");
      if (user == NULL) user = getenv("USERNAME");
      if (user == NULL) user = getenv("USER");
      if (user == NULL) {
        fprintf(stderr, "Failed to determine user name\n");
        return 1;
      }
      /* handle 'su -' (getlogin() returns original user on Linux) */
      pwd = getpwnam(user);
      if (pwd != NULL && pwd->pw_uid != getuid()) {
        pwd = getpwuid(getuid());
        if(pwd != NULL)
          user = pwd->pw_name;
      }
    }

    /* determine terminal name */
    if (term == NULL) {
        term = getenv("TERM");

        if (term == NULL) term = "vt100";
    }

    /* extract group name information */
    if (groups == NULL) {
        gid_t *list;
        int n;
        groups = "";

        n = getgroups(0, NULL);
        list = (gid_t *) malloc(n*sizeof(gid_t));

        if (list == NULL) {
            fprintf(stderr, "Failed to allocate memory.");
            exit(1);
        }

        if ((n = getgroups(n, list)) > 0) {
            struct group *g;
            int i;
            int size = 0;

            for(i=0 ; i < n ; i++) {
                g = getgrgid(list[i]);
                if (g != NULL) size += strlen(g->gr_name)+1;
            }

            groups = (char *) malloc(size);

            if (groups == NULL) {
                fprintf(stderr, "Failed to allocate memory.");
                exit(1);
            }

            groups[0] = '\0';

            for(i=0 ; i < n ; i++) {
                g = getgrgid(list[i]);
                if (g != NULL) {
                    strncat(groups, g->gr_name, size);
                    if (i != n-1)
                        strncat(groups, ",", size);
                }
            }
        }
    }

    /* extract group gid information */
    if (gids == NULL) {
        gid_t *list;
        int n, i;

        n = getgroups(0, NULL);
        list = (gid_t *) malloc(n*sizeof(gid_t));

        if (list == NULL) {
            fprintf(stderr, "Failed to allocate memory.");
            exit(1);
        }


        if ((n = getgroups(n, list)) > 0) {
            gids = (int *) malloc(sizeof(int) * n);

            if (gids == NULL) {
                fprintf(stderr, "Failed to allocate memory.");
                exit(1);
            }

            gidsn = n;

            for(i = 0 ; i < n ; i++)
                gids[i] = list[i];
        }
    }

    /* send version */
    write_fill_confd(fd,  CLISTART_PROTO_VSN, strlen(CLISTART_PROTO_VSN));
    write_fill_confd(fd,  ";", 1);
    if (verbose) fprintf(stderr, "cli: sending version\n");

    /* send username */
    write_fill_confd(fd,  user, strlen(user));
    write_fill_confd(fd,  ";", 1);
    if (verbose) fprintf(stderr, "cli: sending username\n");

    /* send ip */
    write_fill_confd(fd,  ip, strlen(ip));
    write_fill_confd(fd,  ";", 1);
    if (verbose) fprintf(stderr, "cli: sending ip\n");

    /* send proto type (ssh, tcp, console) */
    write_fill_confd(fd,  ctype, strlen(ctype));
    write_fill_confd(fd,  ";", 1);
    if (verbose) fprintf(stderr, "cli: sending type\n");

    /* send opaque (as "special" first group for back/forward compat) */
    if (opaque != NULL) {
        int i, len = strlen(opaque);

        for (i = 0; i < len; i++) {
            /* turn comma into NUL since it is separator in group list */
            if (opaque[i] == ',')
                opaque[i] = '\0';
        }
        write_fill_confd(fd, "OPAQUE=", strlen("OPAQUE="));
        write_fill_confd(fd, opaque, len);
        write_fill_confd(fd,  ",", 1);
        if (verbose) fprintf(stderr, "cli: sending opaque\n");
    }

    /* send groups */
    write_fill_confd(fd,  groups, strlen(groups));
    write_fill_confd(fd,  ";", 1);
    if (verbose) fprintf(stderr, "cli: sending groups\n");

    /* send host name */
    write_fill_confd(fd, "HOST=", strlen("HOST="));
    if (host != NULL) {
      write_fill_confd(fd,  host, strlen(host));
    }
    write_fill_confd(fd,  ";", 1);
    if (verbose) fprintf(stderr, "cli: sending hostname\n");

    /* send ttyname */
    write_fill_confd(fd,  tty_name, strlen(tty_name));
    write_fill_confd(fd,  ";", 1);
    if (verbose) fprintf(stderr, "cli: sending ttyname\n");

    /* send current working directory */
    write_fill_confd(fd,  cwd, strlen(cwd));
    write_fill_confd(fd,  ";", 1);
    if (verbose) fprintf(stderr, "cli: sending cwd\n");

    /* send terminal */
    write_fill_confd(fd,  term, strlen(term));
    write_fill_confd(fd,  ";", 1);
    if (verbose) fprintf(stderr, "cli: sending terminal\n");

    /* send SSH_CONNECTION */
    if (ssh_connection) {
        write_fill_confd(fd,  ssh_connection, strlen(ssh_connection));
        write_fill_confd(fd,  ";", 1);
    }
    else {
        write_fill_confd(fd,  ";", 1);
    }
    if (verbose) fprintf(stderr, "cli: sending ssh_connection info\n");

    /* send cli style */
    write_fill_confd(fd, style, strlen(style));
    write_fill_confd(fd,  ";", 1);
    if (verbose) fprintf(stderr, "cli: sending CLI style\n");

    /* send user identity (uid) */
    write_int(fd, uid);
    if (verbose) fprintf(stderr, "cli: sending uid\n");

    /* send user gid (gid) */
    write_int(fd, gid);
    if (verbose) fprintf(stderr, "cli: sending gid\n");

    /* send users secondary groups */
    write_int(fd, gidsn);
    {
        int i;
        for(i=0 ; i < gidsn ; i++)
            write_int(fd, gids[i]);
    }
    if (verbose) fprintf(stderr, "cli: sending gids\n");

    /* activate raw mode */
    interactive = isatty(infd);

    if (interactive_send == -1)
        interactive_send = interactive;

    if (interactive) {
        tty_raw(infd);
        signal(SIGWINCH, sig_handler);
        get_size();
        if (old_raw) {
            nl = nl_interactive;
            nl_len = 2;
        } else {
            nl = nl_batch;
            nl_len = 1;
        }
    }
    else {
        nl = nl_batch;
        nl_len = 1;
    }

    if (height == 0) height = 24;
    if (width == 0) width = 80;

    write_int(fd,  width);
    if (verbose) fprintf(stderr, "cli: sending width%s", nl);
    write_int(fd,  height);
    if (verbose) fprintf(stderr, "cli: sending height%s", nl);
    write_int(fd, interactive_send);
    if (verbose) fprintf(stderr, "cli: sending interactive%s", nl);
    write_int(fd, noaaa);
    if (verbose) fprintf(stderr, "cli: sending noaaa%s", nl);
    write_int(fd, stop_on_error);
    if (verbose) fprintf(stderr, "cli: sending stop_on_error %s", nl);

    /**********************************************************************
     * Set up sockets for main proxy loop, enter non-blocking mode
     */

    fds[0].fd = infd;
    fds[0].events = POLLIN;

    fds[1].fd = fd;
    fds[1].events = POLLIN;

    fds[2].fd = outfd;
    fds[2].events = 0;

    nfds = 3;

    set_nonblocking(fd, outfd);

    /* Tunnel IO */
    while(1) {
        int r;

        r = poll(fdsptr, nfds, -1);

        if (verbose && r < 0) perror("poll");

        if (r < 0 && interactive && !(fds[1].events & POLLOUT)) {
            /* Check for term resize event */
            if (read_term_sz == 1) {
                read_term_sz = 0;
                get_size();
                if (verbose) {
                    fprintf(stderr, "sigwinch %d %d%s", width, height, nl);
                }
                put_int32(width, &out0buf[2]);
                put_int32(height, &out0buf[6]);
                out0buf[0] = 0;  /* ESC char */
                out0buf[1] = INBAND_SIGWINCH;
                write_fill_confd_noesc(fd,  out0buf, 10);
            }
            continue;
        }

        /* check if we are waiting for confd to be ready to accept
         * more data
         */
        if (fds[1].revents & POLLOUT && fds[1].events & POLLOUT) {
            /* ready, write */

            /* restore poll flags, write_fill_confd_noesc will
             * re-set them if we block again
             */
            fds[1].events = fds[1].events & ~POLLOUT;
            fds[0].events = fds[0].events | POLLIN;

            /* write remaining unwritten data */
            write_fill_confd_noesc(fd, block0, block0_remain);
        }

        /* check if we are waiting for terminal to be ready to accept
         * more data
         */
        if (fds[2].revents & POLLOUT && fds[2].events & POLLOUT) {
            int w;
            /* ready, write */

            /* restore poll flags, write_fill_term will re-set them if
             * we block again
             */
            fds[2].events = fds[2].events & ~POLLOUT;
            fds[1].events = fds[1].events | POLLIN;

            w = write_fill_term(outfd, block1, block1_remain);

            if (w < 0) {
                perror("write");
                if (verbose)
                    fprintf(stderr, "cli: write error to stdout%s", nl);
                goto error;
            }
        }

        /* Data from terminal side? */
        if (fds[0].revents & (POLLIN | POLLHUP) && fds[0].events & POLLIN) {
            int n, w, i;

            n = read(fds[0].fd, in0buf, 1024);
            if (n < 0 && errno != ECONNRESET) {
                perror("read");
                if (verbose)
                    fprintf(stderr, "cli: error reading from stdin%s", nl);
                goto error;
            }
            else if (n == 0 || (n < 0 && errno == ECONNRESET)) {
                if (verbose)
                    fprintf(stderr, "cli: read close on stdin%s", nl);

                if (interactive) {
                    goto error;
                }
                else {
                    shutdown(fds[1].fd, SHUT_WR);
                    /* wait for the server to close */
                    if (fdsptr == fds) {
                        fdsptr++;
                        nfds--;
                    }
                    fds[0].revents = 0;
                }
            }

            /* Scan for global panic character: three consecutive
             *  ctrl-_
             */
            for(i=0 ; i < n && n > 0 ; i++) {
                if (in0buf[i] == 31) {
                    escape_count++;
                    if (escape_count == 3) {
                        if (verbose)
                            fprintf(stderr, "cli: read escape sequence%s", nl);
                        exit_code=6;
                        goto error;
                    }
                }
                else if (escape != -1 && in0buf[i] == escape) {
                    exit_code=6;
                    goto error;
                }
                else {
                    escape_count = 0;
                }
            }

            if (n > 0) {
                w = write_fill_confd(fd,  in0buf, n);
                if (w < 0) {
                    perror("write");
                    if (verbose)
                        fprintf(stderr, "cli: write error to server%s", nl);
                    goto error;
                }
            }
        }
        else if (fds[0].revents & ~POLLOUT && fds[0].events != 0) {
            if (verbose)
                fprintf(stderr, "cli: error events on stdin%s", nl);
            if (interactive)
                goto error;
            else {
                shutdown(fds[1].fd, SHUT_WR);
                /* wait for the server to close */
                if (fdsptr == fds) {
                    fdsptr++;
                    nfds--;
                }
                fds[0].revents = 0;
            }
        }

        /* Data from ConfD? */
        if (fds[1].revents & POLLIN && fds[1].events & POLLIN) {
            int n, w, i, j;

            n = read(fds[1].fd, in1buf, 1024);
            if (n < 0 && errno != ECONNRESET) {
                perror("read");
                if (verbose)
                    fprintf(stderr, "cli: read error from server%s", nl);
                goto error;
            }
            else if (n == 0 || (n < 0 && errno == ECONNRESET)) {
                if (verbose)
                    fprintf(stderr, "cli: read close from server%s", nl);
                goto error;
            }

            /* escape \n with \r\n */
            for(i=0,j=0 ; i < n ; i++,j++) {
                if (next_may_be_exit_code) {
                    next_may_be_exit_code=0;
                    if (in1buf[i] != '\0') {
                        exit_code = (int) ((unsigned char)in1buf[i]);
                        if (exit_code ==  254)
                            exit_code=0;
                        if (verbose)
                            fprintf(stderr, "cli: setting exit code: %d%s",
                                    exit_code, nl);
                        j--;
                    } else
                        out1buf[j]=in1buf[i];
                }
                else if (in1buf[i] == '\n' &&
                    nl_len == 2) {
                    out1buf[j++] = nl[0];
                    out1buf[j]   = nl[1];
                }
                else if (in1buf[i] == '\0') {
                    next_may_be_exit_code=1;
                    j--;
                }
                else
                    out1buf[j] = in1buf[i];
            }

            w = write_fill_term(outfd, out1buf, j);

            if (w < 0) {
                perror("write");
                if (verbose)
                    fprintf(stderr, "cli: write error to stdout%s", nl);
                goto error;
            }
        }
        else if ((fds[1].revents & ~POLLOUT && fds[1].events != 0) &&
                 (fds[2].revents & ~POLLOUT && fds[2].events != 0) ) {
            if (verbose)
                fprintf(stderr, "cli: error on server socket%s", nl);
            goto error;
        }
    }

 error:
    if (interactive) {
#ifdef NCURSES
        endwin();
#else
        tty_restore(1);
#endif
    }

    restore_blocking(fd, outfd);

    return exit_code;
}
