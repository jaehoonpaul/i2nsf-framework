###############################################################################
# ConfD Subscriber intro example
# Implements a DHCP server adapter
#
# (C) 2005-2007 Tail-f Systems
# Permission to use this code as a starting point hereby granted
#
# See the README file for more information
###############################################################################
from __future__ import print_function
import os
import socket
import threading
import time

import _confd
import _confd.cdb as cdb
import dhcpd_ns


def duration2secs(duration):
    duration_string = str(duration)
    # Note that this function is not complete, it only handles seconds
    [seconds, skipped] = duration_string.strip('PT').rsplit('S')
    return seconds


class Subscriber:
    def __init__(self, prio=100, path='/'):
        self.sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM, 0)
        self.path = path
        self.prio = prio

        cdb.connect(self.sock, cdb.SUBSCRIPTION_SOCKET, '127.0.0.1',
                    _confd.CONFD_PORT, self.path)
        cdb.subscribe(self.sock, self.prio, dhcpd_ns.ns.hash, self.path)
        cdb.subscribe_done(self.sock)

        print("Subscribed to {path}".format(path=self.path))

    def subscribeloop(self):
        self.wait()
        self.read_confd()
        self.ack()

    def wait(self):
        cdb.read_subscription_socket(self.sock)

    def ack(self):
        cdb.sync_subscription_socket(self.sock, cdb.DONE_PRIORITY)

    def read_confd(self):

        rsock = socket.socket(socket.AF_INET, socket.SOCK_STREAM, 0)

        ns = dhcpd_ns.ns
        cdb.connect(rsock, cdb.READ_SOCKET, '127.0.0.1',
                    _confd.CONFD_PORT, '/')
        cdb.start_session(rsock, cdb.RUNNING)
        cdb.set_namespace(rsock, ns.hash)

        default_lease_time = cdb.get(rsock, "/dhcp/default-lease-time")
        max_lease_time = cdb.get(rsock, "/dhcp/max-lease-time")
        log_facility = cdb.get(rsock, "/dhcp/log-facility")

        if log_facility == ns.dhcpd_kern:
            log_facility_string = "log-facility kern"
        if log_facility == ns. dhcpd_mail:
            log_facility_string = "log-facility mail"
        if log_facility == ns.dhcpd_local7:
            log_facility_string = "log-facility local7"

        self.fp = open("dhcpd.conf.tmp", "w")
        self.fp.write("default-lease-time {dlt}\n"
                      "max-lease-time {mlt}\n"
                      "{lfs}\n".format(dlt=duration2secs(default_lease_time),
                                       mlt=duration2secs(max_lease_time),
                                       lfs=log_facility_string))

        sub_nets = cdb.num_instances(
            rsock,
            "/dhcp/subnets/subnet")
        for i in range(0, sub_nets):
            cdb.cd(rsock, "/dhcp/subnets/subnet[{index}]".format(index=i))
            self.do_subnet(rsock)

        shared_networks = cdb.num_instances(
            rsock,
            "/dhcp/shared-networks/shared-network"
        )
        for i in range(0, shared_networks):
            sh_net = "/dhcp/shared-networks/shared-network[{0}]".format(str(i))
            network_name = cdb.get(
                rsock,
                sh_net + "/name"
            )

            self.fp.write("shared-network {0} {{\n".format(str(network_name)))

            m = cdb.num_instances(
                rsock,
                sh_net + "/subnets/subnet")
            for j in range(0, m):
                cdb.pushd(rsock, sh_net +
                          "/subnets/subnet[{0}]".format(str(j)))
                self.do_subnet(rsock)
                cdb.popd(rsock)

            self.fp.write("}\n")
        self.fp.close()
        return cdb.close(rsock)

    def do_subnet(self, rsock):
        net = cdb.get(rsock, "net")
        mask = cdb.get(rsock, "mask")

        self.fp.write("subnet {net} netmask {netmask} {{\n".format(
            net=str(net), netmask=str(mask)))

        if cdb.exists(rsock, "range"):
            self.fp.write(" range ")
            dynamic_bootp = cdb.get(rsock, "range/dynamic-bootp")
            if dynamic_bootp:
                self.fp.write(" dynamic-bootp ")
            low_addr = cdb.get(rsock, "range/low-addr")
            hi_addr = cdb.get(rsock, "range/hi-addr")
            self.fp.write(" {low}  {high} \n".format(
                low=str(low_addr),
                high=str(hi_addr)
            ))

        if cdb.exists(rsock, "routers"):
            routers = cdb.get(rsock, "routers")
            comma_routers = str(routers).replace(" ", ",")
            self.fp.write(" option routers {0}\n".format(comma_routers))

        mlt = cdb.get(rsock, "max-lease-time")
        self.fp.write(" max-lease-time {0}\n}};\n".format(duration2secs(mlt)))


def run():

    # Setup subscription
    sub = Subscriber(10, '/dhcp')

    # Read Initial config
    sub.read_confd()

    def subscriberfun():
        while (True):
            os.rename("dhcpd.conf.tmp", "dhcpd.conf")
            # This is the place to HUP the daemon
            print("Configuration applied to dhcpd")
            sub.subscribeloop()
            print("Configuration changed")

    threading.Thread(target=subscriberfun).start()

    try:
        while (True):
            time.sleep(0.1)

    except KeyboardInterrupt:
        print("\nCtrl-C pressed\n")


if __name__ == "__main__":
    run()
