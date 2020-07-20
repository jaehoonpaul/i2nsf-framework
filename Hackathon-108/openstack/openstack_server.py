import os
import sys
import socket
import threading
import time


def receive_nsf_request():
    while True:
        server_socket = socket.socket()
        server_socket.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        server_socket.bind(('115.145.178.180', 55560))
        server_socket.listen(5)
        client_socket, addr = server_socket.accept()
        data = client_socket.recv(1024)
        temp_data = data.split(",")

        for i in range(0, len(temp_data)):
            # print("tacker vnf-create --vnfd-name " + temp_data[i] + "_vnfd "
            # + temp_data[i] + "_vnf")
            os.system("tacker vnf-create --vnfd-name " +
                      temp_data[i] + "_vnfd " + temp_data[i] + "_vnf")
            time.sleep(5)

        # print("tacker vnffg-create --vnffgd-template vnffg_test.yaml
        # block_sns")
        time.sleep(90)
        os.system(
            "tacker vnffg-create --vnffgd-template vnffg_test.yaml block_sns")

threading._start_new_thread(receive_nsf_request, ())

print("\nStarting Openstack Server...\n")

while True:
    pass
