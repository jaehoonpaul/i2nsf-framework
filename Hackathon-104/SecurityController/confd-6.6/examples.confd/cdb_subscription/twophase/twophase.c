/*
 * Copyright 2012 Tail-f Systems AB
 *
 * Permission to use this code as a starting point hereby granted
 *
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <signal.h>
#include <sys/poll.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <sys/un.h>
#include <assert.h>
#include <time.h>
#include <confd.h>
#include <confd_cdb.h>

static char *progname;
static enum confd_debug_level debug = CONFD_SILENT;
static FILE *debugf = NULL;
static struct sockaddr_in addr;        /* Keeps address to confd daemon */

#define OK(E) assert((E) == CONFD_OK)

int danger_flag;

static enum cdb_iter_ret do_iter(confd_hkeypath_t *kp,
                                 enum cdb_iter_op op,
                                 confd_value_t *oldv,
                                 confd_value_t *newv,
                                 void *state)
{
    char tmppath[BUFSIZ];
    char tmpbuf1[BUFSIZ], tmpbuf2[BUFSIZ];
    char *opstr = "";
    char *prefix = (char *)state;
    confd_pp_kpath(tmppath, BUFSIZ, kp);
    switch (op) {
    case MOP_CREATED:   opstr = "created";  break;
    case MOP_DELETED:   opstr = "deleted";  break;
    case MOP_VALUE_SET: opstr = "set";      break;
    case MOP_MODIFIED:  opstr = "modified"; break;
    case MOP_MOVED_AFTER: opstr = "Moved"; break;
    default: opstr = "?";
    }

    tmpbuf1[0] = tmpbuf2[0] = 0;
    if (oldv) confd_pp_value(tmpbuf1, BUFSIZ, oldv);
    if (newv) confd_pp_value(tmpbuf2, BUFSIZ, newv);

    printf("%s%s %s", prefix ? prefix : "", tmppath, opstr);
    if (oldv || newv) {
        printf(" (%s -> %s)", tmpbuf1, tmpbuf2);
    }

    printf("\n");
    return ITER_RECURSE;
}

static void call_diff_iterate(int s, char *info, int *subp, int len,
                              int schema_order)
{
    int i;
    int diflags = ITER_WANT_PREV;
    if (schema_order) diflags |= ITER_WANT_SCHEMA_ORDER;
    for (i=0; i<len; i++) {
        int id = *(subp+i);
        char prefix[32];
        snprintf(prefix, sizeof(prefix), "%s %d ", info, id);
        cdb_diff_iterate(s, id, do_iter, diflags, prefix);
    }
}

static int cmp_subid(const void *ap, const void *bp)
{
    int *a = (int *)ap;
    int *b = (int *)bp;
    if (*a == *b) return 0;
    if (*a > *b)  return 1;
    return -1;
}

static char *notif2str(enum cdb_sub_notification type)
{
    switch (type) {
    case CDB_SUB_PREPARE: return "PREPARE";
    case CDB_SUB_COMMIT:  return "COMMIT";
    case CDB_SUB_ABORT:   return "ABORT";
    case CDB_SUB_OPER:    return "OPER";
    }
    return NULL;
}


enum alloc_cmd {
    ALLOC_SUCCEED = 0,
    ALLOC_FAIL = 1,
    ALLOC_RANDOM = 2,
    ALLOC_SUBID = 3
};

int go_check_hardware()
{
  /*A function. To check if the hardware is ready to be configured*/


  char *inname = "hardware.txt";
  FILE *infile;
  char line_buffer[5];

  if (!(infile = fopen(inname, "r"))) {
    printf("Couldn't check hardware (%s)\n", inname);
    return 0;
  }
  fgets(line_buffer, sizeof(line_buffer), infile);
  fclose(infile);

  return atoi(line_buffer);
}




static int get(char *path, int lock, confd_value_t *v)
{
    int s = socket(PF_INET, SOCK_STREAM, 0);
    OK(cdb_connect(s, CDB_DATA_SOCKET,
                   (struct sockaddr *)&addr, sizeof(addr)));
    if (lock) {
        OK(cdb_start_session2(s, CDB_RUNNING, CDB_LOCK_SESSION));
    } else {
        OK(cdb_start_session2(s, CDB_RUNNING, 0));
    }
    OK(cdb_get(s, v, path));
    OK(cdb_end_session(s));
    OK(cdb_close(s));
    return 0;
}

static int pget(char *path, int lock)
{
    confd_value_t v;
    char b[64];
    get(path, lock, &v);
    b[0] = '\000';
    confd_pp_value(b, sizeof(b), &v);
    printf("get(%s) -> %s\n", path, b);
    return 0;
}


int main(int argc, char *argv[])
{
    int ss, c;
    char *confd_addr = "127.0.0.1";
    int confd_port = CONFD_PORT;
    enum cdb_sub_type subtype = CDB_SUB_RUNNING_TWOPHASE;
    int prio = 10;
    int di = 1;
    int di_schema_order = 0;
    char *gp = NULL;

    /* Setup progname (without path component) */
    if ((progname = strrchr(argv[0], (int)'/')) == NULL)
      progname = argv[0];
    else
      progname++;

    {
      char *ptmp = getenv("CONFD_IPC_PORT");
      if (ptmp) {
        confd_port = atoi(ptmp);
      } else {
        confd_port = CONFD_PORT;
      }
    }

    /* Parse command line */
    while ((c = getopt(argc, argv, "da:p:intr:o:semfg:")) != EOF) {
        switch (c) {
        case 'd':
          debug++;
          break;
        case 'a':
          confd_addr = optarg;
          break;
        case 'p':
          confd_port = atoi(optarg);
          break;
        case 'g':// try to Get path after notification
          gp = optarg;
          break;
        default:
          exit(1);
        }
    }
    argc -= optind;
    argv += optind;

    if (argc < 1) {
      fprintf(stderr, "%s: Need at least one path to subscribe to!\n",
              progname);
      exit(1);
    }

    /* Initialize address to confd daemon */
    {
      struct in_addr in;
      if (inet_pton(AF_INET, confd_addr, &in) == 1) {
        addr.sin_family = AF_INET;
        addr.sin_addr.s_addr = in.s_addr;
        addr.sin_port = htons(confd_port);
      } else {
        fprintf(stderr, "unparsable adress: %s\n", confd_addr);
        exit(1);
      }
    }

    /* always save trace output somewhere */
    if (debug == CONFD_SILENT) {
      char fname[255];
        char *suffix = getenv("CDB_SET_FILE_SUFFIX");
        if (confd_port == CONFD_PORT) {
          snprintf(fname, sizeof(fname), "_tmp_%s", progname);
        } else {
          snprintf(fname, sizeof(fname), "_tmp_%s.%d", progname, confd_port);
        }
        if (suffix) {
          char tmpstr[16];
          if (strcmp(suffix, "pid") == 0) {
            snprintf(tmpstr, sizeof(tmpstr), "%d", (int)getpid());
            suffix = tmpstr;
          }
          strncat(fname, suffix, sizeof(fname) - strlen(fname) - 1);
        }
        debugf = fopen(fname, "w");
        debug = CONFD_TRACE;
    } else {
      debugf = stderr;
    }

    confd_init(progname, debugf, debug);
    OK(confd_load_schemas((struct sockaddr *)&addr, sizeof(addr)));

    ss = socket(PF_INET, SOCK_STREAM, 0);
    OK(cdb_connect(ss, CDB_SUBSCRIPTION_SOCKET,
                   (struct sockaddr *)&addr, sizeof(addr)));
    OK(cdb_mandatory_subscriber(ss, "mandatory"));
    int ret;
    int sid;
    ret = cdb_subscribe2(ss, subtype, 0, prio, &sid, 0, argv[0]);
    OK(ret);

    OK(cdb_subscribe_done(ss));

    for (;;) {
      struct pollfd set[1];
      int p;

      set[0].fd = ss;
      set[0].events = POLLIN;
      set[0].revents = 0;
      //        int ji =1;
      p = poll(set, 1, -1);
        if (p < 0) {
          perror("poll() failed:");
          continue;
        }

        if (set[0].revents & POLLIN) {
          int res, flags, len, *subp;
          enum cdb_sub_notification type;

          res = cdb_read_subscription_socket2(ss, &type, &flags, &subp, &len);

          if (res == CONFD_EOF) {
            exit(0);
          }
          OK(res);

          /* to make the results predictable for testing, sort
           * subscription points */
          qsort(subp, len, sizeof(*subp), cmp_subid);
            {
              int i;
              printf("\nrecv notification %s flags=", notif2str(type));
              if (flags && CDB_SUB_FLAG_IS_LAST) { printf("LAST"); }
              printf(" subids=");
              for(i=0; i<len; i++) { printf("%d ", *(subp + i)); }
              printf("\n");
            }

            switch (type) {
            case CDB_SUB_PREPARE:
              printf("\nTYPE=CDB_SUB_PREPARE\n");
              {
                if (di) call_diff_iterate(ss, "diff prepare", subp, len,
                                          di_schema_order);
                if (gp) pget(gp, 0);

                if (go_check_hardware() == 0) {
                  printf("send abort\n");
                  cdb_sub_abort_trans(ss, CONFD_ERRCODE_RESOURCE_DENIED,
                                      0, 0, "HARDWARE IS NOT CONFIGURABLE");
                }else {
                  printf("send CDB_DONE_PRIORITY\n");
                  cdb_sync_subscription_socket(ss, CDB_DONE_PRIORITY);

                }
                break;
              }
            case CDB_SUB_COMMIT:
              {
                printf("\nTYPE=CDB_SUB_COMMIT\n");
                /*  Configuration has now changed */
                if (di) call_diff_iterate(ss, "diff commit", subp, len,
                                          di_schema_order);
                if (gp) pget(gp, 1);

                printf("send CDB_DONE_PRIORITY\n");
                cdb_sync_subscription_socket(ss, CDB_DONE_PRIORITY);
                break;
            }
            case CDB_SUB_ABORT:
                /* transaction was aborted, release resources.... */
                printf("send CDB_DONE_PRIORITY\n");
                cdb_sync_subscription_socket(ss, CDB_DONE_PRIORITY);
                break;
            case CDB_SUB_OPER:
                assert(0);
                break;
            }
            if (subp) { free(subp); }
        }
    }
}
