#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <sys/poll.h>
#include <time.h>
#include <sys/time.h>

#include <confd_lib.h>
#include <confd_dp.h>

#include "notif.h"

#define OK(val) (assert((val) == CONFD_OK))
static int ctlsock, workersock;
static struct confd_daemon_ctx *dctx;

struct notif {
    struct confd_datetime eventTime;
    confd_tag_value_t *vals;
    int nvals;
};

/* Our replay buffer is kept in memory in this example.  It's a circular
 * buffer of struct notif.
 */
#define MAX_BUFFERED_NOTIFS 4
static struct notif replay_buffer[MAX_BUFFERED_NOTIFS];
static unsigned int first_replay_idx = 0;
static unsigned int next_replay_idx = 0;

static struct confd_datetime replay_creation;
static int replay_has_aged_out = 0;
static struct confd_datetime replay_aged_time;

#define MAX_REPLAYS 10
struct replay {
    int active;
    int started;
    unsigned int idx;
    struct confd_notification_ctx *ctx;
    struct confd_datetime start;
    struct confd_datetime stop;
    int has_stop;
};
/* Keep tracks of active replays */
static struct replay replay[MAX_REPLAYS];

/* The notification context (filled in by ConfD) for the live feed */
static struct confd_notification_ctx *live_ctx;

static int get_ctlsock(struct addrinfo *addr)
{
    int sock;

    if ((sock =
         socket(addr->ai_family, addr->ai_socktype, addr->ai_protocol)) < 0)
        return -1;
    if (confd_connect(dctx, sock, CONTROL_SOCKET,
                      addr->ai_addr, addr->ai_addrlen) != CONFD_OK) {
        close(sock);
        return -1;
    }
    return sock;
}

static int get_workersock(struct addrinfo *addr)
{
    int sock;

    if ((sock =
         socket(addr->ai_family, addr->ai_socktype, addr->ai_protocol)) < 0)
        return -1;
    if (confd_connect(dctx, sock, WORKER_SOCKET,
                      addr->ai_addr, addr->ai_addrlen) != CONFD_OK) {
        close(sock);
        return -1;
    }
    return sock;
}

static int log_times(struct confd_notification_ctx *nctx)
{
    struct confd_datetime *aged;

    if (replay_has_aged_out)
        aged = &replay_aged_time;
    else
        aged = NULL;

    return confd_notification_reply_log_times(nctx, &replay_creation, aged);
}

static void getdatetime(struct confd_datetime *datetime)
{
    struct tm tm;
    struct timeval tv;

    gettimeofday(&tv, NULL);
    gmtime_r(&tv.tv_sec, &tm);

    memset(datetime, 0, sizeof(*datetime));
    datetime->year = 1900 + tm.tm_year;
    datetime->month = tm.tm_mon + 1;
    datetime->day = tm.tm_mday;
    datetime->sec = tm.tm_sec;
    datetime->micro = tv.tv_usec;
    datetime->timezone = 0;
    datetime->timezone_minutes = 0;
    datetime->hour = tm.tm_hour;
    datetime->min = tm.tm_min;
}

static void send_notification(confd_tag_value_t *vals, int nvals)
{
    int sz;
    struct confd_datetime now;
    struct notif *notif;

    getdatetime(&now);
    notif = &replay_buffer[next_replay_idx];
    if (notif->vals) {
        /* we're aging out this notification */
        replay_has_aged_out = 1;
        replay_aged_time = notif->eventTime;
        first_replay_idx = (first_replay_idx + 1) % MAX_BUFFERED_NOTIFS;
        free(notif->vals);
    }
    notif->eventTime = now;
    sz = nvals * sizeof(confd_tag_value_t);
    notif->vals = malloc(sz);
    memcpy(notif->vals, vals, sz);
    notif->nvals = nvals;
    next_replay_idx = (next_replay_idx + 1) % MAX_BUFFERED_NOTIFS;
    OK(confd_notification_send(live_ctx,
                               &notif->eventTime,
                               notif->vals,
                               notif->nvals));
}

static void send_notifup_1(int index, int flags1, int flags2)
{
    confd_tag_value_t vals[9];
    int i = 0;

    CONFD_SET_TAG_XMLBEGIN(&vals[i], notif_linkUp,       notif__ns);  i++;
    CONFD_SET_TAG_UINT32(&vals[i],   notif_ifIndex,      index);      i++;
    CONFD_SET_TAG_XMLBEGIN(&vals[i], notif_linkProperty, notif__ns);  i++;
    CONFD_SET_TAG_UINT32(&vals[i],   notif_flags,        flags1);     i++;
    CONFD_SET_TAG_XMLEND(&vals[i],   notif_linkProperty, notif__ns);  i++;
    CONFD_SET_TAG_XMLBEGIN(&vals[i], notif_linkProperty, notif__ns);  i++;
    CONFD_SET_TAG_UINT32(&vals[i],   notif_flags,        flags2);     i++;
    CONFD_SET_TAG_XMLEND(&vals[i],   notif_linkProperty, notif__ns);  i++;
    CONFD_SET_TAG_XMLEND(&vals[i],   notif_linkUp,       notif__ns);  i++;
    send_notification(vals, i);
}

static void send_notifup_2(int index, int flags1, int val1, int val2)
{
    confd_tag_value_t vals[15];
    int i = 0;

    CONFD_SET_TAG_XMLBEGIN(&vals[i], notif_linkUp,       notif__ns);  i++;
    CONFD_SET_TAG_UINT32(&vals[i],   notif_ifIndex,      index);      i++;
    CONFD_SET_TAG_XMLBEGIN(&vals[i], notif_linkProperty, notif__ns);  i++;
    CONFD_SET_TAG_XMLTAG(&vals[i],   notif_newlyAdded,   notif__ns);  i++;
    CONFD_SET_TAG_UINT32(&vals[i],   notif_flags,        flags1);     i++;
    CONFD_SET_TAG_XMLBEGIN(&vals[i], notif_extensions,   notif__ns);  i++;
    CONFD_SET_TAG_UINT32(&vals[i],   notif_name,         1);          i++;
    CONFD_SET_TAG_UINT32(&vals[i],   notif_value,        val1);       i++;
    CONFD_SET_TAG_XMLEND(&vals[i],   notif_extensions,   notif__ns);  i++;
    CONFD_SET_TAG_XMLBEGIN(&vals[i], notif_extensions,   notif__ns);  i++;
    CONFD_SET_TAG_UINT32(&vals[i],   notif_name,         2);          i++;
    CONFD_SET_TAG_UINT32(&vals[i],   notif_value,        val2);       i++;
    CONFD_SET_TAG_XMLEND(&vals[i],   notif_extensions,   notif__ns);  i++;
    CONFD_SET_TAG_XMLEND(&vals[i],   notif_linkProperty, notif__ns);  i++;
    CONFD_SET_TAG_XMLEND(&vals[i],   notif_linkUp,       notif__ns);  i++;
    send_notification(vals, i);
}

static void send_notifdown(int index)
{
    confd_tag_value_t vals[3];
    int i = 0;

    CONFD_SET_TAG_XMLBEGIN(&vals[i], notif_linkDown,     notif__ns);  i++;
    CONFD_SET_TAG_UINT32(&vals[i],   notif_ifIndex,      index);      i++;
    CONFD_SET_TAG_XMLEND(&vals[i],   notif_linkDown,     notif__ns);  i++;
    send_notification(vals, i);
}

static int datetime_le(struct confd_datetime *a, struct confd_datetime *b)
{
    unsigned int ax, bx;
    if (a->year < b->year) { return 1; }
    if (a->year>b->year) { return 0;}
    if (a->month<b->month) { return 1; }
    if (a->month>b->month) { return 0;}
    if (a->day<b->day) { return 1;}
    if (a->day>b->day) { return 0;}
    ax = a->hour - a->timezone;
    bx = b->hour - b->timezone;
    if (ax<bx) { return 1;}
    if (ax>bx) { return 0;}
    ax = a->min - a->timezone_minutes;
    bx = b->min - b->timezone_minutes;
    if (ax<bx) { return 1;}
    if (ax>bx) { return 0;}
    if (a->sec<b->sec) { return 1;}
    if (a->sec>b->sec) { return 0;}
    if (a->micro<b->micro) { return 1;}
    if (a->micro>b->micro) { return 0;}
    return 1;
}

/* Try to start a new replay.  This function just allocates a replay
 * entry; no notifications are sent from this callback.  Notifications
 * are sent from the main poll loop.
 */
static int start_replay(struct confd_notification_ctx *nctx,
                        struct confd_datetime *start,
                        struct confd_datetime *stop)
{
    int rnum;

    for (rnum = 0; rnum < MAX_REPLAYS; rnum++) {
        if (!replay[rnum].active) {
            replay[rnum].active = 1;
            replay[rnum].started = 0;
            replay[rnum].idx = first_replay_idx;
            replay[rnum].ctx = nctx;
            replay[rnum].start = *start;
            if (stop) {
                replay[rnum].has_stop = 1;
                replay[rnum].stop = *stop;
            } else
                replay[rnum].has_stop = 0; /* stop when caught up to live */
            return CONFD_OK;
        }
    }
    confd_notification_seterr(nctx, "Max no. of replay requests reached");
    return CONFD_ERR;
}

static int continue_replay(struct replay *r)
{
    int done = 0;

    if (!r->started) {
        /* search for first notif to send */
        r->idx = first_replay_idx;
        do {
            if (datetime_le(&r->start, &replay_buffer[r->idx].eventTime))
                break;
            r->idx = (r->idx + 1) % MAX_BUFFERED_NOTIFS;
        } while (r->idx != next_replay_idx);
        r->started = 1;
    }

    /* send one until stop time */
    if (!r->has_stop ||
        datetime_le(&replay_buffer[r->idx].eventTime, &r->stop)) {
        printf("sending notif %d\n", r->idx);
        OK(confd_notification_send(r->ctx,
                                   &replay_buffer[r->idx].eventTime,
                                   replay_buffer[r->idx].vals,
                                   replay_buffer[r->idx].nvals));
        r->idx = (r->idx + 1) % MAX_BUFFERED_NOTIFS;
        if (r->idx == next_replay_idx)
            done = 1;
    } else
        done = 1;

    if (done) {
        /* tell ConfD we're done with replay */
        OK(confd_notification_replay_complete(r->ctx));
        r->active = 0;
    }
    return CONFD_OK;
}

int main(int argc, char **argv)
{
    char confd_port[16];
    struct addrinfo hints;
    struct addrinfo *addr = NULL;
    int debuglevel = CONFD_SILENT;
    int i;
    int c;
    char *p, *dname;
    struct confd_notification_stream_cbs ncb;
    struct pollfd set[3];
    int ret;
    int timeout;
    int rnum;

    snprintf(confd_port, sizeof(confd_port), "%d", CONFD_PORT);
    memset(&hints, 0, sizeof(hints));
    hints.ai_family = PF_UNSPEC;
    hints.ai_socktype = SOCK_STREAM;

    while ((c = getopt(argc, argv, "dtprc:")) != -1) {
        switch (c) {
        case 'd':
            debuglevel = CONFD_DEBUG;
            break;
        case 't':
            debuglevel = CONFD_TRACE;
            break;
        case 'p':
            debuglevel = CONFD_PROTO_TRACE;
            break;
        case 'c':
            if ((p = strchr(optarg, '/')) != NULL)
                *p++ = '\0';
            else
                p = confd_port;
            if (getaddrinfo(optarg, p, &hints, &addr) != 0) {
                if (p != confd_port) {
                    *--p = '/';
                    p = "/port";
                } else {
                    p = "";
                }
                fprintf(stderr, "%s: Invalid address%s: %s\n",
                        argv[0], p, optarg);
                exit(1);
            }
            break;
        default:
            fprintf(stderr,
                    "Usage: %s [-dtpr] [-c address[/port]]\n",
                    argv[0]);
            exit(1);
        }
    }

    if (addr == NULL &&
        ((i = getaddrinfo("127.0.0.1", confd_port, &hints, &addr)) != 0))
        /* "Can't happen" */
        confd_fatal("%s: Failed to get address for ConfD: %s\n",
                    argv[0], gai_strerror(i));
    if ((dname = strrchr(argv[0], '/')) != NULL)
        dname++;
    else
        dname = argv[0];
    /* Init library */
    confd_init(dname, stderr, debuglevel);

    if ((dctx = confd_init_daemon(dname)) == NULL)
        confd_fatal("Failed to initialize ConfD\n");
    if ((ctlsock = get_ctlsock(addr)) < 0)
        confd_fatal("Failed to connect to ConfD\n");
    if ((workersock = get_workersock(addr)) < 0)
        confd_fatal("Failed to connect to ConfD\n");

    memset(replay_buffer, 0, sizeof(replay_buffer));
    memset(replay, 0, sizeof(replay));
    getdatetime(&replay_creation);

    memset(&ncb, 0, sizeof(ncb));
    ncb.fd = workersock;
    ncb.get_log_times = log_times;
    ncb.replay = start_replay;
    strcpy(ncb.streamname, "interface");
    ncb.cb_opaque = NULL;
    if (confd_register_notification_stream(dctx, &ncb, &live_ctx) != CONFD_OK) {
        confd_fatal("Couldn't register stream %s\n", ncb.streamname);
    }
    if (confd_register_done(dctx) != CONFD_OK) {
        confd_fatal("Failed to complete registration\n");
    }

    printf("notifier started\n");
    fflush(stdout);

    while (1) {
        set[0].fd = ctlsock;
        set[0].events = POLLIN;
        set[0].revents = 0;

        set[1].fd = 0;
        set[1].events = POLLIN;
        set[1].revents = 0;

        /* if we're doning a replay, don't use a timeout */
        timeout = -1;
        for (rnum = 0; rnum < MAX_REPLAYS; rnum++) {
            if (replay[rnum].active) {
                timeout = 0;
                break;
            }
        }

        switch (poll(set, 2, timeout)) {
        case -1:
            break;

        default:
            /* Check for I/O */
            if (set[0].revents & POLLIN) { /* ctlsock */
                if ((ret = confd_fd_ready(dctx, ctlsock)) == CONFD_EOF) {
                    confd_fatal("Control socket closed\n");
                } else if (ret == CONFD_ERR &&
                           confd_errno != CONFD_ERR_EXTERNAL) {
                    confd_fatal("Error on control socket request: "
                                "%s (%d): %s\n", confd_strerror(confd_errno),
                                confd_errno, confd_lasterr());
                }
            }
            if (set[1].revents & (POLLIN|POLLHUP)) { /* stdin */
                char c;
                if (!read(0, &c, 1))
                    exit(0);
                switch (c) {
                case 'u':
                    printf("sending linkUp notification\n");
                    send_notifup_1(1, 2112, 32);
                    break;
                case 'i':
                    printf("sending linkUp notification\n");
                    send_notifup_1(2, 42, 4668);
                    break;
                case 'y':
                    printf("sending linkUp second notification\n");
                    send_notifup_2(2, 42, 3, 4668);
                    break;
                case 'd':
                    printf("sending linkDown notification\n");
                    send_notifdown(1);
                    break;
                case '\n':
                    break;
                default:
                    printf("unknown character <%c>\n", c);
                    break;
                }
            }
            if (timeout == 0) {
                /* continue replay */
                for (rnum = 0; rnum < MAX_REPLAYS; rnum++) {
                    if (replay[rnum].active) {
                        continue_replay(&replay[rnum]);
                    }
                }
            }
        }
    }
}
