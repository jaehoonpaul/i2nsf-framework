"""
*********************************************************************
* ConfD kicker example                                              *
* Implements a couple of actions                                    *
*                                                                   *
* (C) 2015 Tail-f Systems                                           *
* Permission to use this code as a starting point hereby granted    *
*                                                                   *
* See the README file for more information                          *
*********************************************************************
"""
from __future__ import print_function
import socket
import select
import sys

from example_ns import ns
import _confd
import _confd.dp as dp
import _confd.maapi as maapi
import _confd.error as confd_errors


def connect_maapi():
    s = socket.socket()
    maapi.connect(s, '127.0.0.1', _confd.PORT)
    maapi.load_schemas(s)
    return s


def make_tag_value(ns_hash, tag, value):
    """
    Wrapper to create a _confd.TagValue
    """
    return _confd.TagValue(_confd.XmlTag(ns_hash, tag),
                           _confd.Value(value))

class DiffIterator(object):
    def __init__(self):
        self.count = 0

    def __call__(self, kp, op, oldv, newv):
        print('kp=%s, op=%s, oldv=%s, newv=%s' %
              (str(kp), str(op), str(oldv), str(newv)))
        sys.stdout.flush()
        self.count += 1
        return _confd.ITER_RECURSE


class ActionCallbacks(object):

    """
    The action callbacks needed.
    """

    def __init__(self, worker_sock):
        self.wsock = worker_sock

    def cb_init(self, uinfo):
        print("init_action called")
        sys.stdout.flush()
        dp.action_set_fd(uinfo, self.wsock)

    def cb_action(self, uinfo, name, keypath, params):
        print("action called")
        sys.stdout.flush()
        for i, param in enumerate(params):
            print("param", i, "value", param.v)
            sys.stdout.flush()
        self.keypath = keypath
        self.uinfo = uinfo
        self.name = name
        self.params = params
        fun = self.action_switch()
        action_result = fun()
        if action_result is not None:
            return action_result

    def cb_abort(self, uinfo):
        print("Aborting outstanding action")
        sys.stdout.flush()
        # We need to clean  up the worker socket by replying
        dp.action_delayed_reply_error(uinfo, "aborted")

    def action_switch(self):
        """
        Hacky python switch
        """
        return {
            ns.example_local_me: self.local_me,
            ns.example_kick_me: self.kick_me,
            ns.example_iter_me: self.iter_me,
        }[self.name.tag]

    def iter_me(self):
        """
        tailf:action iter_me handling
        """
        print("::: iter_me :::")
        sys.stdout.flush()
        params = self.params
        uinfo = self.uinfo
        # params[0] is mode
        id_value = str(params[0].v)
        path_value = str(params[1].v)
        tid_value = int(params[2].v)

        s = connect_maapi()
        maapi.attach2(s, 0, 0, tid_value)

        iterator = DiffIterator()
        maapi.diff_iterate(s, tid_value, iterator, 0)

        maapi.detach2(s, tid_value)

        result = []
        dp.action_reply_values(uinfo, result)

    def kick_me(self):
        """
        tailf:action kick_me handling
        """
        print("::: kick_me :::")
        sys.stdout.flush()
        params = self.params
        uinfo = self.uinfo

        result = []
        dp.action_reply_values(self.uinfo, result)

    def local_me(self):
        """
        tailf:action local_me handling
        """
        print("::: local_me :::")
        sys.stdout.flush()
        params = self.params
        uinfo = self.uinfo
        keypath = self.keypath
        print("location:", keypath)
        sys.stdout.flush()

        result = []
        dp.action_reply_values(self.uinfo, result)


def connect(dctx, csock, wsock):
    """
    Connect the sockets
    """
    # Create the first control socket, all requests to
    # create new transactions arrive here
    dp.connect(dx=dctx,
               sock=csock,
               type=dp.CONTROL_SOCKET,
               ip='127.0.0.1',
               port=4565)

    # Also establish a workersocket, this is the most simple
    # case where we have just one ctlsock and one workersock
    dp.connect(dx=dctx,
               sock=wsock,
               type=dp.WORKER_SOCKET,
               ip='127.0.0.1',
               port=4565)


def read_data(dctx, sock):
    try:
        dp.fd_ready(dctx, sock)
    except (confd_errors.Error) as e:
        # Callback error
        if e.confd_errno is _confd.ERR_EXTERNAL:
            print(str(e))
            sys.stdout.flush()
        else:
            raise e


def poll_loop(dctx, ctrl_sock, worker_sock):
    """
    Check for I/O
    """
    _r = [ctrl_sock, worker_sock]
    _w = []
    _e = []

    try:
        while True:
            r, w, e = select.select(_r, _w, _e)

            for rs in r:
                if rs == ctrl_sock:
                    read_data(dctx=dctx, sock=ctrl_sock)
                elif rs == worker_sock:
                    read_data(dctx=dctx, sock=worker_sock)

    except KeyboardInterrupt:
        print("\nCtrl-C pressed\n")
        sys.stdout.flush()


def action_main():
    ctrl_sock = socket.socket()
    worker_sock = socket.socket()

    dctx = dp.init_daemon("actions_daemon")

    connect(dctx=dctx,
            csock=ctrl_sock,
            wsock=worker_sock)

    # register the action handler callback object
    acb = ActionCallbacks(worker_sock=worker_sock)
    dp.register_action_cbs(dctx, 'kick-me-point', acb)

    dp.register_done(dctx)
    print("register_done called")
    sys.stdout.flush()

    try:
        poll_loop(dctx, ctrl_sock, worker_sock)
    finally:
        worker_sock.close()
        ctrl_sock.close()
        dp.release_daemon(dctx)

if __name__ == "__main__":
    action_main()
