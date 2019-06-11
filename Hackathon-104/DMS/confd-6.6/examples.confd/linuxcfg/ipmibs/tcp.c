#include <errno.h>
#include <string.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <malloc.h>

#include <confd.h>

#include "tcp.h"
#include "linuxcfg_api.h"
#include "utils.h"

int get_tcp_stats(struct TcpStats *tcp)
{
//  size_t i;
    int cc;
    char *header_line = NULL;
    char *value_line = NULL;

    char linebuf[BUFSIZ];

    FILE *fp;

    if ((fp = fopen("/proc/net/snmp", "r")) == NULL)
        return -1;

    memset(tcp, 0, sizeof(*tcp));

    /******** Find value line ********/
    if (fgets(linebuf, BUFSIZ, fp) != linebuf)
        warn("Couldn't read first line.");

    for (; fgets(linebuf, BUFSIZ, fp) == linebuf;) {
        if (!header_line) {
            header_line = strstr(linebuf, "Tcp");
        } else {
            value_line = strstr(linebuf, "Tcp");
        }
        if (value_line)
            break;
    }
    fclose(fp);
    if (!value_line) {
        printf("******** Failed to get value line\n");
        return -1;
    }
    int in, out;

    /******** Parse line ********/
    get_int_dec(&value_line, &(tcp->tcpRtoAlgorithm));
    get_int_dec(&value_line, &(tcp->tcpRtoMin));
    get_int_dec(&value_line, &(tcp->tcpRtoMax));
    get_int_dec(&value_line, &(tcp->tcpMaxConn));
    get_int_dec(&value_line, &(tcp->tcpActiveOpens));
    get_int_dec(&value_line, &(tcp->tcpPassiveOpens));
    get_int_dec(&value_line, &(tcp->tcpAttemptFails));
    get_int_dec(&value_line, &(tcp->tcpEstabResets));
    get_int_dec(&value_line, &(tcp->tcpCurrEstab));
    get_int_dec(&value_line, &(in));
    get_int_dec(&value_line, &(out));
    get_int_dec(&value_line, &(tcp->tcpRetransSegs));
    get_int_dec(&value_line, &(tcp->tcpInErrs));
    cc = get_int_dec(&value_line, &(tcp->tcpOutRsts));
    if (cc < 0) {
        printf("******** Failed to parse value line\n");
    }

    UPDATE_COUNTER_PAIR(in, tcp->tcpInSegs, tcp->tcpHCInSegs);
    UPDATE_COUNTER_PAIR(out, tcp->tcpOutSegs, tcp->tcpHCOutSegs);
    return cc;
}

static int get_tcp_connections(TCP_CONNECTION_LIST * connect,
                        TCP_LISTENER_LIST * listen)
{
    int dummy, i;
    char *line = NULL, linebuf[BUFSIZ];
    struct TcpConnection values;
    TCP_LISTENER *listener;
    TCP_CONNECTION *connection;
    int is_zero = 0;

    if (!connect || !listen)
        return -1;

    RESET_LIST(connect);
    RESET_LIST(listen);

    FILE *fp;

    if ((fp = fopen("/proc/net/tcp", "r")) != NULL) {
        if (fgets(linebuf, BUFSIZ, fp) != linebuf)
            warn("Couldn't read first line.");

        for (;fgets(linebuf, BUFSIZ, fp) == linebuf;) {
            memset(&values, 0, sizeof(values));
            line = linebuf;

            get_int_dec(&line, &dummy);
            get_ip_addr_hex(&line, &values.tcpConnectionLocalAddress,
                            &values.tcpConnectionLocalAddressType, &is_zero);
            get_uint_hex(&line, &values.tcpConnectionLocalPort);
            get_ip_addr_hex(&line, &values.tcpConnectionRemAddress,
                            &values.tcpConnectionRemAddressType, &is_zero);
            get_uint_hex(&line, &values.tcpConnectionRemPort);
            get_uint_hex(&line, &values.tcpConnectionState);
            get_uint_hex(&line, (unsigned*)&dummy);
            get_uint_hex(&line, (unsigned*)&dummy);
            get_uint_hex(&line, (unsigned*)&dummy);
            get_uint_hex(&line, (unsigned*)&dummy);
            get_uint_hex(&line, (unsigned*)&dummy);
            get_int_dec(&line, &dummy);
            get_int_dec(&line, &dummy);
            get_uint_dec(&line, &values.tcpInode);

            if (is_zero && values.tcpConnectionRemPort == 0) {
                listener = NEW_ENTRY(listen);
                listener->tcpListenerLocalAddressType =
                    values.tcpConnectionLocalAddressType;
                ADDR_CPY(listener->tcpListenerLocalAddress,
                         values.tcpConnectionLocalAddress);
                listener->tcpListenerLocalPort = values.tcpConnectionLocalPort;
                listener->tcpListenerProcess = values.tcpConnectionProcess;
                listener->tcpInode = values.tcpInode;
            } else {
                connection = NEW_ENTRY(connect);
                memcpy(connection, &values, sizeof(values));
            }
        }
        fclose(fp);
    }
    if ((fp = fopen("/proc/net/tcp6", "r")) != NULL) {
        if (fgets(linebuf, BUFSIZ, fp) != linebuf)
            warn("Couldn't read first line.");
        for (;fgets(linebuf, BUFSIZ, fp) == linebuf;) {
            memset(&values, 0, sizeof(values));
            line = linebuf;

            get_int_dec(&line, &dummy);
            get_ip_addr_hex(&line, &values.tcpConnectionLocalAddress,
                            &values.tcpConnectionLocalAddressType, &is_zero);
            get_uint_hex(&line, &values.tcpConnectionLocalPort);
            get_ip_addr_hex(&line, &values.tcpConnectionRemAddress,
                            &values.tcpConnectionRemAddressType, &is_zero);
            get_uint_hex(&line, &values.tcpConnectionRemPort);
            get_uint_hex(&line, &values.tcpConnectionState);
            get_int_dec(&line, &dummy);
            get_int_dec(&line, &dummy);
            get_int_dec(&line, &dummy);
            get_int_dec(&line, &dummy);
            get_int_dec(&line, &dummy);
            get_int_dec(&line, &dummy);
            get_int_dec(&line, &dummy);
            get_uint_dec(&line, &values.tcpInode);
            values.tcpConnectionLocalAddressType = INET(ipv6);
            values.tcpConnectionRemAddressType = INET(ipv6);

            if (is_zero && values.tcpConnectionRemPort == 0) {
                listener = NEW_ENTRY(listen);
// TODO: also ipv6! (from ./tcp6)
                listener->tcpListenerLocalAddressType = INET(ipv6);
                ADDR_CPY(listener->tcpListenerLocalAddress,
                         values.tcpConnectionLocalAddress);
                listener->tcpListenerLocalPort = values.tcpConnectionLocalPort;
                listener->tcpListenerProcess = values.tcpConnectionProcess;
                listener->tcpInode = values.tcpInode;
            } else {
                connection = NEW_ENTRY(connect);
                memcpy(connection, &values, sizeof(values));
            }
        }
        fclose(fp);
    }

    int total = LIST_SIZE(connect) + LIST_SIZE(listen);
    inode_pid_pair_t pids[total], *lpids, *cpids;

    cpids = pids;
    lpids = pids + LIST_SIZE(connect);
    memset(pids, 0, sizeof(pids));
    SORT_LIST(connect);
    SORT_LIST(listen);

    FOR_ENTRIES(connect, connection, i)
        cpids[i].inode = connection->tcpInode;
    FOR_ENTRIES(listen, listener, i)
        lpids[i].inode = listener->tcpInode;
    find_process_ids(pids, total);
    FOR_ENTRIES(connect, connection, i)
        connection->tcpConnectionProcess = cpids[i].process_id;
    FOR_ENTRIES(listen, listener, i)
        listener->tcpListenerProcess = lpids[i].process_id;
    return 0;
}

void update_tcp_stats(TCP_ENDPOINT_STATS * stats)
{
    int rv;

    TRACE_ENTRY();
    if ((rv = get_tcp_connections(&stats->connections, &stats->listeners)) != 0)
        error("Tcp update failed, return value %d", rv);
    TRACE_EXIT();
}

void update_tcp_table(TCP_STATS * stats)
{
    int rv;

    TRACE_ENTRY();
    if ((rv = get_tcp_stats(stats)) != 0)
        error("Tcp update failed, return value %d", rv);
    TRACE_EXIT();
}

int compare_tcp_connections(const TCP_CONNECTION * s1,
                            const TCP_CONNECTION * s2)
{
    return
        UINT_CMP(s1->tcpConnectionLocalAddressType,
                 s2->tcpConnectionLocalAddressType) ? :
        (INET_ADDR_CMP(s1->tcpConnectionLocalAddress,
                       s2->tcpConnectionLocalAddress) ? :
         (UINT_CMP(s1->tcpConnectionLocalPort,
                   s2->tcpConnectionLocalPort) ? :
          (UINT_CMP(s1->tcpConnectionRemAddressType,
                    s2->tcpConnectionRemAddressType) ? :
           (INET_ADDR_CMP(s1->tcpConnectionRemAddress,
                          s2->tcpConnectionRemAddress) ? :
            UINT_CMP(s1->tcpConnectionRemPort, s2->tcpConnectionRemPort)))));
}

int compare_tcp_listeners(const TCP_LISTENER * l1, const TCP_LISTENER * l2)
{
    return
        UINT_CMP(l1->tcpListenerLocalAddressType,
                 l2->tcpListenerLocalAddressType) ? :
        (INET_ADDR_CMP(l1->tcpListenerLocalAddress,
                       l2->tcpListenerLocalAddress) ? :
         UINT_CMP(l1->tcpListenerLocalPort,
                  l2->tcpListenerLocalPort));
}
