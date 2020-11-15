import os
import sys
import socket
import threading
import time
import logging

logger = logging.getLogger("dms")
logger.setLevel(logging.INFO)
stream_handler = logging.StreamHandler()
logger.addHandler(stream_handler)

clock_bool = 0
NSF_num = 0
created_NSF_num = 0
logger.info("DEBUG1")
# Security Controller -> DMS -> Stack User

def receive_nsf_request():
	logger.info("receive_nsf_request")
	global NSF_num
	global clock_bool
	while True:
		server_socket = socket.socket()
		server_socket.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
		server_socket.bind(('127.0.0.1', 55562)) #EDIT TO YOUR DMS IP ADDRESS
		server_socket.listen(5)
		client_socket, addr = server_socket.accept()
		data = client_socket.recv(1024)
		logger.info("Requested NSF: %s\n" % data)

		HOST = '127.0.0.1'  #EDIT TO YOUR OPENSTACK IP ADDRESS
		PORT = 55564 # OPENSTACK PORT
		ADDR = (HOST,PORT)
		client_socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
		try:
			client_socket.connect(ADDR)
			client_socket.send(data)
			NSF_num = len(data.split(","))
			clock_bool = 1
			logger.info("\n       Requested NSF Information")
			logger.info("Requested NSF num : %d" % NSF_num)
			logger.info("Requested NSF: %s\n" % data)
		except Exception as e:
			logger.info("%s:%s" % ADDR)
			sys.exit()

# NSFs -> DMS -> Security Controller
def receive_nsf_ip():
	logger.info("receive_nsf_ip")
	global created_NSF_num
	global NSF_num
	global clock_bool
	while True:
		server_socket = socket.socket()
		server_socket.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
		server_socket.bind(('127.0.0.1', 55561))  #EDIT TO YOUR DMS IP ADDRESS
		server_socket.listen(5)
		client_socket, addr = server_socket.accept()
		data = client_socket.recv(1024)
		temp_data = data.split(",")
		HOST = '127.0.0.1'  #EDIT TO YOUR SECURITY CONTROLLER IP ADDRESS
		PORT = 55560
		ADDR = (HOST,PORT)
		client_socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
		try:
			client_socket.connect(ADDR)
			client_socket.send(data)
			created_NSF_num += 1
			logger.info("\nCreate NSF!!!")
			logger.info("Created NSF Num: %d" % created_NSF_num)		
			logger.info("   Created NSF Information")
			logger.info("NSF Name: %s" % temp_data[0])
			logger.info("NSF IP: %s\n" % temp_data[1])

			if (NSF_num == created_NSF_num):
				created_NSF_num = 0
				clock_bool = 0
				logger.info("\n#######  Complete creation of all NSFs  ######")

		except Exception as e:
		        logger.info("%s:%s" % ADDR)
		        sys.exit()


def create_clock():
	logger.info("create_clock")
	temp_time = 0
	global clock_bool
	while True:
		time.sleep(1)
		if clock_bool == 1:
			logger.info("[%d] Creating NSF..." % temp_time)
			time.sleep(1)
			temp_time += 1
		else:
			temp_time = 0	

logger.info("DEBUG2")

os.system("/home/ubuntu/confd-6.6/bin/netconf-console --host 127.0.0.1 /home/ubuntu/xml_files/general_firewall.xml")  #EDIT TO YOUR SECURITY CONTROLLER IP ADDRESS
os.system("/home/ubuntu/confd-6.6/bin/netconf-console --host 127.0.0.1 /home/ubuntu/xml_files/time_based_firewall.xml")  #EDIT TO YOUR SECURITY CONTROLLER IP ADDRESS
os.system("/home/ubuntu/confd-6.6/bin/netconf-console --host 127.0.0.1 /home/ubuntu/xml_files/web_filter.xml") #EDIT TO YOUR SECURITY CONTROLLER IP ADDRESS
os.system("/home/ubuntu/confd-6.6/bin/netconf-console --host 127.0.0.1 /home/ubuntu/xml_files/voip_volte_filter.xml") #EDIT TO YOUR SECURITY CONTROLLER IP ADDRESS
os.system("/home/ubuntu/confd-6.6/bin/netconf-console --host 127.0.0.1 /home/ubuntu/xml_files/http_and_https_flood_mitigation.xml") #EDIT TO YOUR SECURITY CONTROLLER IP ADDRESS

threading._start_new_thread(receive_nsf_request, ())
threading._start_new_thread(receive_nsf_ip, ())
threading._start_new_thread(create_clock, ())
logger.info("\nStarting DMS...\n")


while True:
        pass

