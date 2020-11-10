import os
import sys
import socket
import threading


def request_nsf():
		HOST = '10.0.0.6'
		PORT = 55560
		ADDR = (HOST,PORT)
		client_socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
		try:
		        client_socket.connect(ADDR)
		        client_socket.send("time_based_firewall,url_filtering")
		except Exception as e:
		        print("%s:%s" % ADDR)
		        sys.exit()



def receive_nsf_ip():
        while True:
                server_socket = socket.socket()
                server_socket.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
                server_socket.bind(('10.0.0.27', 55560))
                server_socket.listen(5)
                client_socket, addr = server_socket.accept()
                data = client_socket.recv(1024)
		temp_data = data.split(",")
		#print("sudo /home/ubuntu/confd-6.6/bin/netconf-console --host " + temp_data[1] + " /home/ubuntu/" + temp_data[0] + ".xml")
		os.system("sudo /home/ubuntu/confd-6.6/bin/netconf-console --host " + temp_data[1] + " /home/ubuntu/" + temp_data[0] + ".xml")



threading._start_new_thread(receive_nsf_ip, ())
threading._start_new_thread(request_nsf, ())

while True:
        pass

