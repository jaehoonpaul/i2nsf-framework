#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <sys/poll.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <stdarg.h>

#include <confd_lib.h>
#include <confd_events.h>
#include <confd_maapi.h>

#define OK(E) assert((E) == CONFD_OK)

#define YANG_TYPES_URI "urn:ietf:params:xml:ns:yang:ietf-yang-types"

static int notif_socket;
static struct sockaddr_in addr;

static char *attr_str(confd_value_t *v)
{
    switch (CONFD_GET_UINT32(v)) {
    case CONFD_ATTR_TAGS:       return "TAGS";
    case CONFD_ATTR_ANNOTATION: return "ANNOTATIONS";
    case CONFD_ATTR_INACTIVE:   return "INACTIVE";
    }
    return NULL;
}

/* my iteration function */
static enum maapi_iter_ret iter(confd_hkeypath_t *kp,
                                enum maapi_iter_op op,
                                confd_value_t *oldv,
                                confd_value_t *v,
                                void *state)
{
    char path[BUFSIZ];
    char value[BUFSIZ];
    char *opstr;
    struct confd_cs_node *node;
    confd_hkeypath_t *dkp;
    int i;

    confd_pp_kpath(path, sizeof(path), kp);
    value[0] = 0;
    switch (op) {
    case MOP_CREATED:
        opstr = "created";
        break;
    case MOP_DELETED:
        opstr = "deleted";
        break;
    case MOP_MODIFIED:
        opstr = "modified";
        break;
    case MOP_VALUE_SET:
        opstr = "value_set";
        node = confd_find_cs_node(kp, kp->len);
        confd_val2str(node->info.type, v, value, sizeof(value));
        break;
    case MOP_MOVED_AFTER:
        if (v == NULL) {
            opstr = "moved first";
        } else {
            opstr = "moved after";
            /* create+print a hkeypath for the entry this one was moved after */
            dkp = confd_hkeypath_dup(kp);
            for (i = 0; v[i].type != C_NOEXISTS; i++) {
                confd_free_value(&dkp->v[0][i]);
                confd_value_dup_to(&v[i], &dkp->v[0][i]);
            }
            confd_pp_kpath(value, sizeof(value), dkp);
            confd_free_hkeypath(dkp);
        }
        break;
    case MOP_ATTR_SET:
        if (v[1].type == C_NOEXISTS) {
            opstr = "attr_del";
            snprintf(value, sizeof(value), "%s", attr_str(&v[0]));
        } else {
            opstr = "attr_set";
            i = snprintf(value, sizeof(value), "%s -> ", attr_str(&v[0]));
            confd_pp_value(&value[i], sizeof(value) - i, &v[1]);
        }
        break;
    }
    printf ("ITER %s %s %s\n", path, opstr, value);
    return ITER_RECURSE;
}

static void handle_diff_notif(struct confd_trans_ctx *tctx)
{
    /* first we need a maapi socket */
    int maapi_socket;

    if ((maapi_socket = socket(PF_INET, SOCK_STREAM, 0)) < 0 )
        confd_fatal("Failed to open socket\n");

    if (maapi_connect(maapi_socket, (struct sockaddr*)&addr,
                      sizeof (struct sockaddr_in)) < 0)
        confd_fatal("Failed to confd_connect() to confd \n");

    /* no namespace needed for this */
    OK(maapi_attach(maapi_socket, -1, tctx));

    /* Now we can iterate through the currently hanging transaction */
    /* and read out all the diffs */
    OK(maapi_diff_iterate(maapi_socket, tctx->thandle, iter,
                          ITER_WANT_ATTR, NULL));

    /* and finally call done to release data and let  */
    /* the transaction finish */

    OK(confd_diff_notification_done(notif_socket, tctx));
    close(maapi_socket);
}

static void print_commit_notif(enum confd_dbname database,
                               struct confd_user_info *uinfo,
                               int flags)
{
    printf("%scommit on db %d from user %s usid %d\n",
           (flags & CONFD_NOTIF_COMMIT_FLAG_CONFIRMED_EXTENDED) ?
           "extended confirmed " : (flags & CONFD_NOTIF_COMMIT_FLAG_CONFIRMED) ?
           "confirmed " : "",
           database, uinfo->username, uinfo->usid);
}

static void prindent(int n)
{
    int i;

    for (i = 0; i < n; i++)
        printf("  ");
}

static void print_stream_notif_event(struct confd_stream_notification *s)
{
    confd_value_t event_time;
    char value[BUFSIZ];
    int i, indent = 1;

    CONFD_SET_DATETIME(&event_time, s->event_time);
    confd_pp_value(value, sizeof(value), &event_time);
    printf("Notif stream: event at %s\n", value);
    for (i = 0; i < s->nvalues; i++) {
        switch (CONFD_GET_TAG_VALUE(&s->values[i])->type) {
        case C_XMLBEGIN:
            prindent(indent);
            printf("%s\n", confd_hash2str(CONFD_GET_TAG_TAG(&s->values[i])));
            indent++;
            break;
        case C_XMLEND:
            indent--;
            break;
        case C_XMLTAG:
            prindent(indent);
            printf("%s\n", confd_hash2str(CONFD_GET_TAG_TAG(&s->values[i])));
            break;
        default:
            confd_pp_value(value, sizeof(value),
                           CONFD_GET_TAG_VALUE(&s->values[i]));
            prindent(indent);
            printf("%s %s\n", confd_hash2str(CONFD_GET_TAG_TAG(&s->values[i])),
                   value);
        }
    }
}


static void help(char *p)
{
    printf(
"%s usage:\n"
"    -d   (daemon log messages)\n"
"    -D   (developer log messages)\n"
"    -a   (audit log messages)\n"
"    -N   (NETCONF log messages)\n"
"    -S   (SNMP log messages)\n"
"    -t   (takeover mode for syslog)\n"
"    -u   (user session messages)\n"
"    -i   (commit diff messages - iterate over commit diff)\n"
"    -f   (commit failed messages)\n"
"    -C   (confirmed-commit messages)\n"
"    -P   (commit progress messages)\n"
"    -h   (ha information messages)\n"
"    -s   (subagent messages)\n"
"    -F   (forward information messages)\n"
"    -U   (upgrade event messages)\n"
"    -A   (all of the above)\n"
"    -H   (heartbeat messages)\n"
"    -e   (health check messages)\n"
"    -c   (commit simple messages)\n"
"    -n 'name' (notification stream event messages for stream 'name')\n"
"    -y   (make audit log synchronous)\n"
"    -Y   (make ha info synchronous)\n"
"    -r   (request confirmation before sync reply)\n"
"    -T 'interval' (interval for heartbeat / health check in seconds)\n"
"    -B 'time' (start time for notification stream - yang:date-and-time form)\n"
"    -E 'time' (stop time for notification stream - yang:date-and-time form)\n"
"    -x 'filter' (XPath filter for notification stream)\n"
"    -z 'usid' (User session id for AAA restriction on notification stream)\n"
"    -p 'port' (connect to ConfD at 'port')\n", p);
    exit(1);
}


int main(int argc, char **argv)
{
    int c;
    int events = 0;
    int port = CONFD_PORT;
    char *p;
    struct confd_notifications_data dt;
    char *start_time = NULL, *stop_time = NULL;
    struct confd_type *dt_type;
    int confirm_sync = 0;
    char buf[BUFSIZ];

    if ((p = getenv("CONFD_IPC_PORT")) != NULL) {
        port = atoi(p);
    }
    setvbuf(stdout, NULL, _IOLBF, BUFSIZ);
    memset(&dt, 0, sizeof(dt));
    dt.heartbeat_interval = 3000;
    dt.health_check_interval = 10000;
    CONFD_SET_NOEXISTS(&dt.start_time);
    CONFD_SET_NOEXISTS(&dt.stop_time);

    while ((c = getopt(argc, argv,
                       "adtciuhsfSFNDHCUPyYren:AT:B:E:x:z:p:")) != -1) {
        switch(c) {
        case 'a': events |= CONFD_NOTIF_AUDIT; break;
        case 'd': events |= CONFD_NOTIF_DAEMON; break;
        case 't': events |= CONFD_NOTIF_TAKEOVER_SYSLOG; break;
        case 'c': events |= CONFD_NOTIF_COMMIT_SIMPLE; break;
        case 'i': events |= CONFD_NOTIF_COMMIT_DIFF; break;
        case 'u': events |= CONFD_NOTIF_USER_SESSION; break;
        case 'h': events |= CONFD_NOTIF_HA_INFO; break;
        case 's': events |= CONFD_NOTIF_SUBAGENT_INFO; break;
        case 'f': events |= CONFD_NOTIF_COMMIT_FAILED; break;
        case 'S': events |= CONFD_NOTIF_SNMPA; break;
        case 'F': events |= CONFD_NOTIF_FORWARD_INFO; break;
        case 'N': events |= CONFD_NOTIF_NETCONF; break;
        case 'D': events |= CONFD_NOTIF_DEVEL; break;
        case 'H': events |= CONFD_NOTIF_HEARTBEAT; break;
        case 'C': events |= CONFD_NOTIF_CONFIRMED_COMMIT; break;
        case 'U': events |= CONFD_NOTIF_UPGRADE_EVENT; break;
        case 'P': events |= CONFD_NOTIF_COMMIT_PROGRESS; break;
        case 'y': events |= CONFD_NOTIF_AUDIT_SYNC; break;
        case 'e': events |= CONFD_NOTIF_HEALTH_CHECK; break;
        case 'Y': events |= CONFD_NOTIF_HA_INFO_SYNC; break;
        case 'n':
             events |= CONFD_NOTIF_STREAM_EVENT;
             dt.stream_name = optarg;
             break;
        case 'A':
            events =
                CONFD_NOTIF_AUDIT |
                CONFD_NOTIF_DAEMON |
                CONFD_NOTIF_TAKEOVER_SYSLOG |
                CONFD_NOTIF_COMMIT_DIFF |
                CONFD_NOTIF_USER_SESSION |
                CONFD_NOTIF_HA_INFO |
                CONFD_NOTIF_SUBAGENT_INFO |
                CONFD_NOTIF_COMMIT_FAILED |
                CONFD_NOTIF_SNMPA |
                CONFD_NOTIF_FORWARD_INFO |
                CONFD_NOTIF_NETCONF |
                CONFD_NOTIF_DEVEL |
                CONFD_NOTIF_CONFIRMED_COMMIT |
                CONFD_NOTIF_UPGRADE_EVENT |
                CONFD_NOTIF_COMMIT_PROGRESS;
                break;
        case 'T':
            dt.heartbeat_interval = 1000 * atoi(optarg);
            dt.health_check_interval = 1000 * atoi(optarg);
            break;
        case 'B':
            start_time = optarg;
            break;
        case 'E':
            stop_time = optarg;
            break;
        case 'x':
            dt.xpath_filter = optarg;
            break;
        case 'z':
            dt.usid = atoi(optarg);
            break;
        case 'p':
            port = atoi(optarg);
            break;
        case 'r':
            confirm_sync = 1;
            break;
        default:
            help(argv[0]);
        }
    }
    if (events == 0)
        help(argv[0]);

    addr.sin_addr.s_addr = inet_addr("127.0.0.1");
    addr.sin_family = AF_INET;
    addr.sin_port = htons(port);

    confd_init(argv[0], stderr, CONFD_SILENT);
    if (confd_load_schemas((struct sockaddr*)&addr,
                           sizeof (struct sockaddr_in)) != CONFD_OK)
        confd_fatal("Failed to load schemas from confd\n");
    if (start_time != NULL || stop_time != NULL) {
        if (start_time == NULL)
            confd_fatal("Can't give stream stop time without start time\n");
        if ((dt_type = confd_find_ns_type(confd_str2hash(YANG_TYPES_URI),
                                          "date-and-time")) == NULL)
            confd_fatal("Failed to find yang:date-and-time type definition\n");
        if (confd_str2val(dt_type, start_time, &dt.start_time) != CONFD_OK)
            confd_fatal("Invalid start time for stream: %s\n", start_time);
        if (stop_time != NULL &&
            confd_str2val(dt_type, stop_time, &dt.stop_time) != CONFD_OK)
            confd_fatal("Invalid stop time for stream: %s\n", stop_time);
    }
    if ((notif_socket = socket(PF_INET, SOCK_STREAM, 0)) < 0)
        confd_fatal("Failed to open notif socket\n");
    if (confd_notifications_connect2(notif_socket, (struct sockaddr*)&addr,
                                     sizeof (struct sockaddr_in),
                                     events, &dt) != CONFD_OK) {
        confd_fatal("Failed to confd_notifications_connect2() to confd:\n"
                    "confd_errno=%s (%d) confd_lasterr()=%s\n",
                    confd_strerror(confd_errno), confd_errno, confd_lasterr());
    }
    printf("Waiting for event notifications...\n");

    while (1) {
        struct pollfd set[1];
        struct confd_notification n;
        set[0].fd = notif_socket;
        set[0].events = POLLIN;
        set[0].revents = 0;

        if (poll(&set[0], 1, -1) < 0) {
            perror("Poll failed:");
            continue;
        }

        if (set[0].revents & POLLIN) {
            switch (confd_read_notification(notif_socket, &n)) {
            case CONFD_OK:
                break;
            case CONFD_EOF:
                printf("Notification socket closed\n");
                exit(0);
            default:
                printf("Failed to read notification\n");
                exit(1);
            }
            switch(n.type) {
            case CONFD_NOTIF_AUDIT:
                printf("audit: sym=%d/%s, user=%s usid=%d msg=%s\n",
                       n.n.audit.logno,
                       confd_log_symbols[n.n.audit.logno],
                       n.n.audit.user, n.n.audit.usid, n.n.audit.msg);
                if (events & CONFD_NOTIF_AUDIT_SYNC) {
                    if (confirm_sync) {
                        printf("Hit <Enter> for sync:");
                        fgets(buf, sizeof(buf), stdin);
                    }
                    confd_sync_audit_notification(notif_socket, n.n.audit.usid);
                }
                break;
            case CONFD_NOTIF_DAEMON:
            case CONFD_NOTIF_NETCONF:
            case CONFD_NOTIF_DEVEL:
                printf("syslog: sym=%d/%s prio=%d msg=%s\n",
                       n.n.syslog.logno,
                       confd_log_symbols[n.n.syslog.logno],
                       n.n.syslog.prio,
                       n.n.syslog.msg);
                break;
            case CONFD_NOTIF_COMMIT_SIMPLE:
                print_commit_notif(n.n.commit.database,
                                   &n.n.commit.uinfo,
                                   n.n.commit.flags);
                break;
            case CONFD_NOTIF_COMMIT_DIFF:
                print_commit_notif(n.n.commit_diff.database,
                                   &n.n.commit_diff.uinfo,
                                   n.n.commit_diff.flags);
                if (n.n.commit_diff.comment[0] != '\0') {
                    printf("comment: \"%s\"\n", n.n.commit_diff.comment);
                }
                if (n.n.commit_diff.label[0] != '\0') {
                    printf("label: \"%s\"\n", n.n.commit_diff.label);
                }
                handle_diff_notif(n.n.commit_diff.tctx);
                break;
            case CONFD_NOTIF_USER_SESSION:
                printf("user session: type=%d, user=%s usid=%d db=%d\n",
                       n.n.user_sess.type,
                       n.n.user_sess.uinfo.username,
                       n.n.user_sess.uinfo.usid,
                       n.n.user_sess.database);
                break;
            case CONFD_NOTIF_HA_INFO: {
                struct confd_ha_notification *hnot = &n.n.hnot;
                switch(hnot->type) {
                case CONFD_HA_INFO_NOMASTER: {
                    int error = hnot->data.nomaster;
                    /* why don't we have a master */
                    switch (error) {
                    case CONFD_ERR_HA_NOTICK:
                        printf("NOMASTER: notick\n");
                        break;
                    case CONFD_ERR_HA_CLOSED:
                        printf("NOMASTER: closed\n");
                        break;
                    }
                    break;
                }
                case CONFD_HA_INFO_SLAVE_DIED: {
                    char tbuf[255];
                    struct confd_ha_node *dst = &hnot->data.slave_died;
                    confd_pp_value(tbuf, 255, &dst->nodeid);
                    printf("Slave %s died\n", tbuf);
                    break;
                }
                case CONFD_HA_INFO_SLAVE_ARRIVED: {
                    char tbuf[255];
                    struct confd_ha_node *dst = &hnot->data.slave_arrived;
                    confd_pp_value(tbuf, 255, &dst->nodeid);
                    printf("Slave %s arrived\n", tbuf);
                    break;
                }
                case CONFD_HA_INFO_SLAVE_INITIALIZED:
                    printf("Slave is initialized (cdbcopy=%d)\n",
                           hnot->data.cdb_initialized_by_copy);
                    break;
                case CONFD_HA_INFO_IS_MASTER:
                    printf("We are now master \n");
                    break;
                case CONFD_HA_INFO_IS_NONE:
                    printf("We are now none \n");
                    break;
                case CONFD_HA_INFO_BESLAVE_RESULT: {
                    if (hnot->data.beslave_result == 0) {
                        printf("Async beslave() succeeded\n");
                    } else {
                        printf("Async beslave() failed, confd_errno=%d\n",
                               hnot->data.beslave_result);
                    }
                    break;
                }
                default:
                    printf("bad type \n");
                    exit(2);
                }
                if (events & CONFD_NOTIF_HA_INFO_SYNC) {
                    if (confirm_sync) {
                        printf("Hit <Enter> for sync:");
                        fgets(buf, sizeof(buf), stdin);
                    }
                    confd_sync_ha_notification(notif_socket);
                }
                break;
            }
            case CONFD_NOTIF_SUBAGENT_INFO:
                if (n.n.subagent.type == CONFD_SUBAGENT_INFO_UP) {
                    printf("Subagent %s is up\n",
                           n.n.subagent.name);
                }
                else if (n.n.subagent.type == CONFD_SUBAGENT_INFO_DOWN) {
                    printf("Subagent %s is down\n",
                           n.n.subagent.name);
                }
                break;
            case CONFD_NOTIF_COMMIT_FAILED: {
                printf("Commit failed, system is in unclear state \n");
                struct confd_commit_failed_notification *cf = &n.n.cfail;
                switch (cf->provider) {
                case CONFD_DP_CDB:
                    printf("Provider = CDB\n");
                    break;
                case CONFD_DP_NETCONF: {
                    char dst[INET6_ADDRSTRLEN];
                    struct confd_netconf_failed_commit *nc = &cf->v.nc;
                    struct confd_ip *ip = &nc->ip;
                    if (ip->af == AF_INET)
                        inet_ntop(ip->af, &ip->ip.v4, dst, INET6_ADDRSTRLEN);
                    else
                        inet_ntop(ip->af, &ip->ip.v6, dst, INET6_ADDRSTRLEN);
                    printf("Provider = NETCONF @ %s:%d\n", dst, nc->port);
                    break;
                }
                case CONFD_DP_EXTERNAL:
                    printf("Provider = EXTERNAL, DaemonName = %s\n",
                           cf->v.daemon_name);
                    break;
                case CONFD_DP_SNMPGW:
                    printf("Provider = SNMPGW\n");
                    break;
                case CONFD_DP_JAVASCRIPT:
                    printf("Provider = JAVASCRIPT\n");
                    break;
                }
                break;
            }
            case CONFD_NOTIF_SNMPA: {
                char dst[INET6_ADDRSTRLEN];
                struct confd_ip *ip = &n.n.snmpa.ip;
                if (ip->af == AF_INET)
                    inet_ntop(ip->af, &ip->ip.v4, dst, INET6_ADDRSTRLEN);
                else
                    inet_ntop(ip->af, &ip->ip.v6, dst, INET6_ADDRSTRLEN);
                /* much more information available,
                   see snmpa/4-snmp-audit example */
                printf("snmpa: pdu_type=%d request_id=%d addr=%s:%d\n",
                       n.n.snmpa.pdu_type, n.n.snmpa.request_id,
                       dst, n.n.snmpa.port);
                confd_free_notification(&n);
                break;
            }
            case CONFD_NOTIF_FORWARD_INFO: {
                printf("forward info: type %d target %s\n",
                       n.n.forward.type,
                       n.n.forward.target);
                break;
            }
            case CONFD_NOTIF_HEARTBEAT:
                printf("tick heartbeat\n");
                break;
            case CONFD_NOTIF_HEALTH_CHECK:
                printf("tick health check\n");
                break;
            case CONFD_NOTIF_CONFIRMED_COMMIT: {
                char *t;
                switch (n.n.confirm.type) {
                  case CONFD_CONFIRMED_COMMIT:
                      t = "confirmed";
                      break;
                  case CONFD_CONFIRMING_COMMIT:
                      t = "confirming";
                      break;
                  case CONFD_ABORT_COMMIT:
                      t = "abort";
                      break;
                }
                printf("confirmed-commit: type=%s, timeout=%d, user=%s\n",
                       t,
                       n.n.confirm.timeout,
                       n.n.confirm.uinfo.username);
                break;
            }
            case CONFD_NOTIF_UPGRADE_EVENT:
                switch (n.n.upgrade.event) {
                case CONFD_UPGRADE_INIT_STARTED:
                    printf("Upgrade: init started\n");
                    break;
                case CONFD_UPGRADE_INIT_SUCCEEDED:
                    printf("Upgrade: init succeeded\n");
                    break;
                case CONFD_UPGRADE_PERFORMED:
                    printf("Upgrade: performed\n");
                    break;
                case CONFD_UPGRADE_COMMITED:
                    printf("Upgrade: committed\n");
                    break;
                case CONFD_UPGRADE_ABORTED:
                    printf("Upgrade: aborted\n");
                    break;
                }
                break;
            case CONFD_NOTIF_COMMIT_PROGRESS:
                printf("Commit progress on db %d usid %d thandle %d:\n %s\n",
                       n.n.progress.database,
                       n.n.progress.usid,
                       n.n.progress.thandle,
                       n.n.progress.msg);
                break;
            case CONFD_NOTIF_STREAM_EVENT:
                switch (n.n.stream.type) {
                case CONFD_STREAM_NOTIFICATION_EVENT:
                    print_stream_notif_event(&n.n.stream);
                    break;
                case CONFD_STREAM_NOTIFICATION_COMPLETE:
                    printf("Notif stream: notificationComplete\n");
                    break;
                case CONFD_STREAM_REPLAY_COMPLETE:
                    printf("Notif stream: replayComplete\n");
                    break;
                case CONFD_STREAM_REPLAY_FAILED:
                    printf("Notif stream: replay failed: %s\n",
                           n.n.stream.replay_error);
                    break;
                }
                confd_free_notification(&n);
                break;
            default:
                printf("Unknown notification type %d\n", n.type);
                exit(2);
            }
        }
    }
}

