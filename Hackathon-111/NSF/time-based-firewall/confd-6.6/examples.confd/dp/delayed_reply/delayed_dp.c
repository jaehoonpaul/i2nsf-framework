/*********************************************************************
 * ConfD delayed data provider reply example
 *
 * (C) 2005-2017 Tail-f Systems
 * Permission to use this code as a starting point hereby granted
 * This is ConfD Sample Code.
 *
 * See the README file for more information
 ********************************************************************/

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
#include <confd_dp.h>

#include "routes.h"

static struct confd_trans_cbs trans;

static struct confd_daemon_ctx *dctx;
static struct confd_data_cbs data;

int ctlsock;
int workersock;

#define GET_ELEM 1
#define GET_NEXT 2
#define INIT     3

static int32_t max_key = 99;
static int32_t cur_key;

static int cbtype;

static struct confd_trans_ctx *stored_ctx = NULL;

static int s_init(struct confd_trans_ctx *tctx)
{
  cbtype = INIT;
  stored_ctx = tctx;
  return CONFD_DELAYED_RESPONSE;
}

static int get_elem(struct confd_trans_ctx *tctx,
                    confd_hkeypath_t *keypath)

{
  int32_t key;

  switch (CONFD_GET_XMLTAG(&keypath->v[0][0])) {
  case r_id:
    /* existence test - don't bother with delay, we need the key */
    key = CONFD_GET_INT32(&keypath->v[1][0]);
    if (key >= 0 && key <= max_key)
      confd_data_reply_value(tctx, &keypath->v[1][0]);
    else
      confd_data_reply_not_found(tctx);
    return CONFD_OK;
    break;
  case r_value:
    cbtype = GET_ELEM;
    break;
  default:
    return CONFD_ERR;
  }
  return CONFD_DELAYED_RESPONSE;
}

static int get_next(struct confd_trans_ctx *tctx,
                    confd_hkeypath_t *kp, long next)
{
  cur_key = next + 1;
  cbtype = GET_NEXT;
  return CONFD_DELAYED_RESPONSE;
}

int main(int argc, char **argv)
{
  struct sockaddr_in addr;
  int debuglevel = CONFD_TRACE;
  int c;
  confd_lib_use_syslog = 1;

  while ((c = getopt(argc, argv, "Ddpto:s")) != -1) {
    switch(c) {
    case 'd':
      debuglevel = CONFD_DEBUG;
      break;
    case 'p':
      debuglevel = CONFD_PROTO_TRACE;
      break;
    case 't':
      debuglevel = CONFD_TRACE;
      break;
    case 's':
      debuglevel = CONFD_SILENT;
      break;
    }
  }

  confd_init("delayed_daemon", stdout, debuglevel);

  /* These are our sesion callbacks */
  trans.init = s_init;
  trans.finish = NULL;

  data.get_elem = get_elem;
  data.get_next = get_next;
  strcpy(data.callpoint, "routescp");

  if ((dctx = confd_init_daemon("delayed_daemon")) == NULL)
    confd_fatal("Failed to initialize confdlib\n");

  if ((ctlsock = socket(PF_INET, SOCK_STREAM, 0)) < 0 )
    confd_fatal("Failed to open ctlsocket\n");

  addr.sin_addr.s_addr = inet_addr("127.0.0.1");
  addr.sin_family = AF_INET;
  addr.sin_port = htons(CONFD_PORT);

  /* Create the first control socket, all requests to */
  /* create new transs arrive here */

  if (confd_connect(dctx, ctlsock, CONTROL_SOCKET, (struct sockaddr*)&addr,
                    sizeof (struct sockaddr_in)) < 0)
    confd_fatal("Failed to confd_connect() to confd\n");

  if (confd_load_schemas((struct sockaddr*)&addr,
                         sizeof (struct sockaddr_in)) != CONFD_OK) {
    confd_fatal("Failed to load schemas\n");
  }

  /* Also establish a workersocket, this is the most simple */
  /* case where we have just one ctlsock and one workersock */

  if ((workersock = socket(PF_INET, SOCK_STREAM, 0)) < 0 )
    confd_fatal("Failed to open workersocket\n");
  if (confd_connect(dctx, workersock, WORKER_SOCKET,(struct sockaddr*)&addr,
                    sizeof (struct sockaddr_in)) < 0) {
    confd_fatal("Failed to confd_connect() to confd\n");
  }

  confd_register_trans_cb(dctx, &trans);

  if (confd_register_data_cb(dctx, &data) == CONFD_ERR)
    confd_fatal("Failed to register data cb \n");

  confd_set_daemon_flags(dctx, 0);

  if (confd_register_done(dctx) != CONFD_OK)
    confd_fatal("Failed to complete registrations\n");

  while(1) {
    struct pollfd set[2];
    int ret;

    set[0].fd = ctlsock;
    set[0].events = POLLIN;
    set[0].revents = 0;

    set[1].fd = workersock;
    set[1].events = POLLIN;
    set[1].revents = 0;

    cbtype = 0;

    if (poll(&set[0], 2, -1) < 0) {
      perror("Poll failed:");
      continue;
    }

    /* Check for I/O */
    if (set[0].revents & POLLIN) {
      if ((ret = confd_fd_ready(dctx, ctlsock)) == CONFD_EOF) {
        confd_fatal("Control socket closed\n");
      } else if (ret == CONFD_ERR && confd_errno != CONFD_ERR_EXTERNAL) {
        confd_fatal("Error on control socket request: %s (%d): %s\n",
                    confd_strerror(confd_errno), confd_errno, confd_lasterr());
      }
    }
    if (set[1].revents & POLLIN) {
      if ((ret = confd_fd_ready(dctx, workersock)) == CONFD_EOF) {
        confd_fatal("Worker socket closed\n");
      } else if (ret == CONFD_ERR && confd_errno != CONFD_ERR_EXTERNAL) {
         confd_fatal("Error on worker socket request: %s (%d): %s\n",
                confd_strerror(confd_errno), confd_errno, confd_lasterr());
      }
    }

    while (cbtype != 0) {
      int cbt = cbtype;
      confd_value_t v;

      /* In this example we do a sleep to demonstrate a delayed reply,
         a real application might communicate with a data source over
         IPC here to get the data that will eventually be replied with
         when the data is available */

      cbtype = 0;
      switch (cbt) {
      case 0:
        break;
      case GET_ELEM:
        /* Delay for a few secs */
        sleep(3);
        /* Reply */
        CONFD_SET_INT32(&v, stored_ctx->thandle);
        confd_data_reply_value(stored_ctx, &v);
        break;
      case GET_NEXT:
        /* Delay a sec */
        sleep(1);
        /* Reply */
        if (cur_key <= max_key) {
          CONFD_SET_INT32(&v, cur_key);
          confd_data_reply_next_key(stored_ctx, &v, 1, cur_key);
        } else {
          confd_data_reply_next_key(stored_ctx, NULL, -1, -1);
        }
        break;
      case INIT:
        /* Delay a sec */
        sleep(1);
        /* Reply */
        confd_trans_set_fd(stored_ctx, workersock);
        confd_delayed_reply_ok(stored_ctx);
        break;
      default:
        fprintf(stderr, "Bad op cbtype=%d \n", cbtype);
      }
    }
  }
}
