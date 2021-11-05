/*
 * Copyright 2005 Tail-F Systems AB
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
#include <confd_maapi.h>
#include <confd_cdb.h>
#include "smp.h"


static void install_keys()
{
    struct sockaddr_in addr;
    int maapisock;

    if ((maapisock = socket(PF_INET, SOCK_STREAM, 0)) < 0 )
        confd_fatal("Failed to open maapisocket\n");

    addr.sin_addr.s_addr = inet_addr("127.0.0.1");
    addr.sin_family = AF_INET;
    addr.sin_port = htons(CONFD_PORT);

    if (maapi_connect(maapisock, (struct sockaddr*)&addr,
                      sizeof (struct sockaddr_in)) != CONFD_OK)
        confd_fatal("Failed to maapi_connect() to confd \n");

    if (maapi_install_crypto_keys(maapisock) != CONFD_OK)
        confd_fatal("failed to install keys \n");
    if (confd_crypto_keys.des3_keys_initialized != 1 ||
        confd_crypto_keys.aes_keys_initialized != 1)
        confd_fatal("No keys found in confd.conf file \n");

    close(maapisock);
}




static int print_db(struct sockaddr_in *addr)
{
    int rsock, i, n;
    char ciphertext[BUFSIZ];
    char outbuf[BUFSIZ];

    if ((rsock = socket(PF_INET, SOCK_STREAM, 0)) < 0 )
        return CONFD_ERR;
    if (cdb_connect(rsock, CDB_READ_SOCKET, (struct sockaddr*)addr,
                    sizeof (struct sockaddr_in)) < 0)
        return CONFD_ERR;
    if (cdb_start_session(rsock, CDB_RUNNING) != CONFD_OK)
        return CONFD_ERR;
    cdb_set_namespace(rsock, smp__ns);
    if ((n = cdb_num_instances(rsock, "/strs/str")) < 0) {
        cdb_end_session(rsock);
        cdb_close(rsock);
        return CONFD_ERR;
    }
    printf ("n = %d\n", n);
    for(i=0; i<n; i++) {
        char tmppath[BUFSIZ];
        printf ("String %d\n", i+1);

        sprintf(tmppath, "/strs/str[%d]/des3_string", i);
        cdb_get_str(rsock, ciphertext, BUFSIZ, tmppath);
        printf("ciphertext: %s\n", ciphertext);
        int len = strlen(ciphertext);
        confd_decrypt(ciphertext, len, outbuf);
        printf("Confd des decrypt '%s'\n", outbuf);

        sprintf(tmppath, "/strs/str[%d]/aes_string", i);
        cdb_get_str(rsock, ciphertext, BUFSIZ, tmppath);
        printf("ciphertext: %s\n", ciphertext);
        len = strlen(ciphertext);
        confd_decrypt(ciphertext, len, outbuf);
        printf("Confd aes decrypt '%s'\n", outbuf);

    }

    cdb_end_session(rsock),
    cdb_close(rsock);
    return CONFD_OK;
}

int main(int argc, char **argv)
{
    struct sockaddr_in addr;
    int subsock;
    int status;
    int spoint;

    setvbuf(stdout, NULL, _IOLBF, BUFSIZ);
    addr.sin_addr.s_addr = inet_addr("127.0.0.1");
    addr.sin_family = AF_INET;
    addr.sin_port = htons(CONFD_PORT);

    confd_init(argv[0], stderr, CONFD_TRACE);

    install_keys();

    if ((subsock = socket(PF_INET, SOCK_STREAM, 0)) < 0 )
        confd_fatal("Failed to open socket\n");
    if (cdb_connect(subsock, CDB_SUBSCRIPTION_SOCKET, (struct sockaddr*)&addr,
                    sizeof (struct sockaddr_in)) < 0)
        confd_fatal("Failed to confd_connect() to confd \n");
    if ((status = cdb_subscribe(subsock, 3, smp__ns, &spoint,"/strs/str"))
        != CONFD_OK) {
        fprintf(stderr, "Terminate: subscribe %d\n", status);
        exit(0);
    }
    if (cdb_subscribe_done(subsock) != CONFD_OK)
        confd_fatal("cdb_subscribe_done() failed");

    printf("Subscription point = %d\n", spoint);

    print_db(&addr);

    while (1) {
        struct pollfd set[1];

        set[0].fd = subsock;
        set[0].events = POLLIN;
        set[0].revents = 0;


        if (poll(set, sizeof(set)/sizeof(*set), -1) < 0) {
            perror("Poll failed:");
            continue;
        }

        /* Check for I/O */
        if (set[0].revents & POLLIN) {
            int sub_points[1];
            int reslen;
            if ((status = cdb_read_subscription_socket(subsock,
                                                       &sub_points[0],
                                                       &reslen)) != CONFD_OK)
                exit(status);
            if (reslen > 0) {
                if ((status = print_db(&addr)) != CONFD_OK)
                    exit(0);
            }


            if ((status = cdb_sync_subscription_socket(subsock,
                                                       CDB_DONE_PRIORITY))
                != CONFD_OK) {
                exit(status);
            }
        }
    }
}
