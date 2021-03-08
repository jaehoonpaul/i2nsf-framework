
#ifndef _CONFD_IPC_ACCESS_H
#define _CONFD_IPC_ACCESS_H 1

extern int confd_ipc_access_get_secret(unsigned char *result, int rsize);

extern int confd_ipc_access_check(int sock, const unsigned char *secret);

#endif
