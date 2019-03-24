/*
 * Copyright 2006 Tail-F Systems AB
 */
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <sys/poll.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

#include <confd_lib.h>
#include <confd_cdb.h>
#include <confd_maapi.h>
#include "smp.h"


static char *progname;
static enum confd_debug_level dbg = CONFD_SILENT;


static void install_keys(struct sockaddr_in *addr)
{
    int maapisock;

    if ((maapisock = socket(PF_INET, SOCK_STREAM, 0)) < 0 )
        confd_fatal("Failed to open maapisocket\n");

    if (maapi_connect(maapisock, (struct sockaddr*)addr,
                      sizeof (struct sockaddr_in)) != CONFD_OK)
        confd_fatal("Failed to maapi_connect() to confd \n");

    if (maapi_install_crypto_keys(maapisock) != CONFD_OK)
        confd_fatal("failed to install keys \n");
    if (confd_crypto_keys.aes_keys_initialized != 1)
        confd_fatal("No AES keys found in confd.conf file \n");

    close(maapisock);
}


/* read the encrypted keys and print them on standard output */
static int get_clear_text(struct sockaddr_in *addr, FILE *f)
{
    int rsock, i, n;

    install_keys(addr);

    if ((rsock = socket(PF_INET, SOCK_STREAM, 0)) < 0 ) {
        confd_fatal("failed to create socket\n");
    }

    if (cdb_connect(rsock, CDB_READ_SOCKET, (struct sockaddr*)addr,
                    sizeof(*addr)) < 0) {
        confd_fatal("failed to connect to confd\n");
    }

    if (cdb_start_session(rsock, CDB_RUNNING) != CONFD_OK) {
        confd_fatal("failed to start cdb session");
    }

    cdb_set_namespace(rsock, smp__ns);

    if ((n = cdb_num_instances(rsock, "/strs/str")) < 0) {
        cdb_end_session(rsock);
        cdb_close(rsock);
        return CONFD_ERR;
    }

    for(i=0; i<n; i++) {
        int nr;
        char cstr[BUFSIZ], dstr[BUFSIZ];

        cdb_get_str(rsock, cstr, sizeof(cstr), "/strs/str[%d]/secret", i);
        cdb_get_int32(rsock, &nr, "/strs/str[%d]/nr", i);
        memset(dstr, 0, sizeof(dstr));
        confd_decrypt(cstr, strlen(cstr), dstr);
        fprintf(f, "/strs/str{%d}/secret=$0$%s\n", nr, dstr);
    }

    cdb_end_session(rsock),
    cdb_close(rsock);
    return CONFD_OK;
}


/* read stdin expeting key=value on every line, use MAAPI to set values */
static int set_values(struct sockaddr_in *addr, FILE *f)
{
    int msock, th;
    int curline = 0;
    struct confd_ip ip;
    const char *groups[] = { "admin" };

    if (feof(f)) return CONFD_OK; /* no need to do any work then... */

    if ((msock = socket(PF_INET, SOCK_STREAM, 0)) < 0 ) {
        confd_fatal("failed to create socket\n");
    }

    if (maapi_connect(msock, (struct sockaddr*)addr, sizeof(*addr)) < 0) {
        confd_fatal("failed to connect to confd\n");
    }

    ip.af = AF_INET;
    inet_pton(AF_INET, "127.0.0.1", &ip.ip.v4);

    if ((maapi_start_user_session(msock, "admin", progname,
                                  groups, sizeof(groups) / sizeof(*groups),
                                  &ip, CONFD_PROTO_TCP) != CONFD_OK)) {
        confd_fatal("failed to start user session");
    }

    if ((th = maapi_start_trans(msock, CONFD_RUNNING, CONFD_READ_WRITE)) < 0) {
        confd_fatal("failed to start trans\n");
    }

    maapi_set_namespace(msock, th, smp__ns);

    /* read lines and use set_elem2 to set key/value on each line */
    for (;;) {
        char *key, *val, line[BUFSIZ];
        if (fgets(line, sizeof(line), f) == NULL) {
            if (feof(f)) break;
            perror("fgets");
            goto abort;
        }
        curline++;
        key = line;
        while ((*key == ' ') || (*key == '\t')) { key++; }
        if (*key == '#') { continue; }
        if ((val = strchr(key, (int)'=')) == NULL) {
            fprintf(stderr, "error:%d: missing '='\n", curline);
            goto abort;
        }
        *val++ = 0; /* NUL terminate the key, make val point to value */
        {
            char *s;            /* remove trailing cr/nl */
            for (s=val; *s; s++)
                if ((*s == '\n') || (*s == '\r')) {
                    *s = '\000';
                    break;
                }
        }
        if (dbg > CONFD_SILENT) {
            fprintf(stderr, "set %s to \"%s\"\n", key, val);
        }
        if (maapi_set_elem2(msock, th, val, key) != CONFD_OK) {
            fprintf(stderr, "error:%d: %s=%s failed: %s\n",
                    curline, key, val, confd_lasterr());
            goto abort;
        }
    }

    if (maapi_apply_trans(msock, th, 0) != CONFD_OK) {
        confd_fatal("apply_trans failed\n");
    }
    maapi_end_user_session(msock);
    close(msock);
    return CONFD_OK;

abort:
    maapi_abort_trans(msock, th);
    maapi_end_user_session(msock);
    close(msock);
    return CONFD_ERR;
}


int main(int argc, char **argv)
{
    char *confd_addr = "127.0.0.1";
    int confd_port = CONFD_PORT;
    struct sockaddr_in addr;
    int c;
    int mode = 0;               /* 1 = get, 2 = set */
    int ecode = 1;
    char *filename = NULL;

    /* Setup progname (without path component) */
    if ((progname = strrchr(argv[0], (int)'/')) == NULL)
        progname = argv[0];
    else
        progname++;

    /* Parse command line */
    while ((c = getopt(argc, argv, "da:p:gsf:")) != EOF) {
        switch (c) {
        case 'd':
            dbg++;
            break;
        case 'a':
            confd_addr = optarg;
            break;
        case 'p':
            confd_port = atoi(optarg);
            break;
        case 'g':
            mode = 1;
            break;
        case 's':
            mode = 2;
            break;
        case 'f':
            filename = optarg;
            break;
        default:
            printf("huh?\n");
            exit(1);
        }
    }
    if (!mode) {
        fprintf(stderr, "%s: must provide either -s or -g\n", progname);
        exit(1);
    }

    /* Initialize address to confd daemon */
    {
        addr.sin_addr.s_addr = inet_addr(confd_addr);
        addr.sin_family = AF_INET;
        addr.sin_port = htons(confd_port);
    }

    /* Initialize confd, cdb, and maapi */
    confd_init(progname, stderr, dbg);

    /* Now do the work! */
    switch (mode) {
    case 1:
    {
        FILE *f = (filename) ? fopen(filename, "w") : stdout;
        ecode = (get_clear_text(&addr, f) == CONFD_OK) ? 0 : 1;
        break;
    }
    case 2:
    {
        FILE *f = (filename) ? fopen(filename, "r") : stdin;
        ecode = (set_values(&addr, f) == CONFD_OK) ? 0 : 1;
        break;
    }
    }

    exit(ecode);
}
