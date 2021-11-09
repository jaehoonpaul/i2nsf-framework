#ifndef __TCP__H__
#define __TCP__H__

#include <stddef.h>
#include <stdio.h>
#include <stdint.h>

#include "caches.h"
#include "utils.h"

/******** TCP Data Typedefs ********/
typedef struct TcpStats {
    int tcpRtoAlgorithm;
    int tcpRtoMin;
    int tcpRtoMax;
    int tcpMaxConn;
    int tcpActiveOpens;
    int tcpPassiveOpens;
    int tcpAttemptFails;
    int tcpEstabResets;
    int tcpCurrEstab;
    int tcpInSegs;
    int tcpOutSegs;
    int tcpRetransSegs;
    int tcpInErrs;
    int tcpOutRsts;
    u_int64_t tcpHCInSegs, tcpHCOutSegs;
} TCP_STATS;

typedef struct TcpConnection {
    int tcpConnectionLocalAddressType;
    inet_address_t tcpConnectionLocalAddress;
    unsigned int tcpConnectionLocalPort;
    int tcpConnectionRemAddressType;
    inet_address_t tcpConnectionRemAddress;
    unsigned int tcpConnectionRemPort;
    unsigned int tcpConnectionState;
    unsigned int tcpConnectionProcess;
    unsigned int tcpInode;
} TCP_CONNECTION;

typedef struct TcpListener {
    int            tcpListenerLocalAddressType;
    inet_address_t tcpListenerLocalAddress;
    unsigned int   tcpListenerLocalPort;
    unsigned int   tcpListenerProcess;
    unsigned int   tcpInode;
} TCP_LISTENER;

typedef ROW_LIST(TCP_CONNECTION) TCP_CONNECTION_LIST;
typedef ROW_LIST(TCP_LISTENER) TCP_LISTENER_LIST;

typedef struct {
    TCP_CONNECTION_LIST connections;
    TCP_LISTENER_LIST listeners;
    TCP_STATS         stats;
} TCP_ENDPOINT_STATS;

/******** Common Support Typedefs ********/
typedef struct FileCopy {
    const char* m_filename;
    char*       m_buf;
    size_t      m_buflen;
} FILE_COPY;

typedef struct FileLines {
    const char* m_filename;
    char**      m_line;
    size_t      m_linecnt;
} FILE_COPY_LINES;

/******** TCP Value Acquisition ********/
int get_tcp_stats(struct TcpStats* tcp);

// tcp per-endpoint stats update handler
void update_tcp_stats(TCP_ENDPOINT_STATS *stats);
ENTRY_COMPARATOR(TCP_CONNECTION, compare_tcp_connections);
ENTRY_COMPARATOR(TCP_LISTENER, compare_tcp_listeners);

void update_tcp_table(TCP_STATS *stats);

#endif
