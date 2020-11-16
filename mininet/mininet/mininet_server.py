import os
import sys
import socket
import threading
import time
import logging

from mininet.net import Containernet
from mininet.node import RemoteController
from mininet.cli import CLI
from mininet.link import TCLink

logger = logging.getLogger("mininet")
logger.setLevel(logging.INFO)
stream_handler = logging.StreamHandler()
logger.addHandler(stream_handler)

def receive_nsf_request():
    while True:
        server_socket = socket.socket()
        server_socket.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        server_socket.bind(('127.0.0.1', 55564)) #EDIT TO YOUR OPENSTACK IP ADDRESS
        server_socket.listen(5)
        client_socket, addr = server_socket.accept()
        data = client_socket.recv(1024).decode('utf_8')
        temp_data = data.split(",")

        # for i in range(0, len(temp_data)):
            # print("tacker vnf-create --vnfd-name " + temp_data[i] + "_vnfd "
            # + temp_data[i] + "_vnf")
            # os.system("tacker vnf-create --vnfd-name " +
            #           temp_data[i] + "_vnfd " + temp_data[i] + "_vnf")
            # logger.info("vnf-create --vnfd-name " + temp_data[i] + "_vnfd " + temp_data[i] + "_vnf")
            # time.sleep(5)

        # print("tacker vnffg-create --vnffgd-template vnffg_test.yaml
        # block_sns")
        # time.sleep(90)
        # os.system(
            # "tacker vnffg-create --vnffgd-template vnffg_test.yaml block_sns")
        #logger.info("vnffg-create --vnffgd-template vnffg_test.yaml block_sns")

        logger.info('iptables -A FORWARD -p tcp -d ' + d3.IP() + ' --dport 80 -j REJECT')
        r1.cmd('iptables -A FORWARD -p tcp -d ' + d3.IP() + ' --dport 80 -j REJECT')

threading._start_new_thread(receive_nsf_request, ())

logger.info("\nStarting Mininet Server...\n")

net = Containernet(controller=RemoteController)

logger.info('*** Adding controller\n')
c0 = net.addController('c0',
			controller=RemoteController,
			ip='127.0.0.1',
			port=6633)

logger.info('*** Adding docker containers\n')
r1 = net.addDocker('r1', ip='10.1.1.1/24', dimage="httpd:test")
d1 = net.addDocker('d1', ip='10.1.1.251/24', dimage="dos:1.0", defaultRoute='via 10.1.1.1', dcmd="flask run --host='0.0.0.0'", ports=[5000], port_bindings={5000:5000})
d2 = net.addDocker('d2', ip='10.1.1.252/24', dimage="httpd:test", defaultRoute='via 10.1.1.1', dcmd="sh run.sh")
d3 = net.addDocker('d3', ip='11.2.2.252/24', dimage="httpd:test", defaultRoute='via 11.2.2.1', dcmd="sh run.sh")
d4 = net.addDocker('d4', ip='11.2.2.253/24', dimage="httpd:test", defaultRoute='via 11.2.2.1', dcmd="sh run.sh")

logger.info('*** Adding switches\n')
s1 = net.addSwitch('s1')
s2 = net.addSwitch('s2')

logger.info('*** Creating links\n')
net.addLink(s1, r1)
net.addLink(s2, r1, params2={"ip":"11.2.2.1/24"})
net.addLink(s1, d1)
net.addLink(s1, d2)
net.addLink(s2, d3)
net.addLink(s2, d4)

logger.info('*** Starting network\n')
net.start()

logger.info('*** Running CLI\n')
CLI(net)

logger.info('*** Stopping network')
net.stop()


# r1 iptables -D FORWARD 1
# r1 iptables -L -v
# r1 iptables -A FORWARD -p tcp -d d2 --dport 80 -j DROP
# r1 iptables -A FORWARD -d d2 -j DROP
