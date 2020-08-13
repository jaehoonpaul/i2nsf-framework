#from socket import *
import socket
import sys
from time import ctime

HOST = '10.0.0.6' #EDIT TO YOUR DMS IP
PORT = 55561
ADDR = (HOST,PORT)
s = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
s.connect(('115.145.178.180',0)) #EDIT TO YOUR OPENSTACK IP
print (socket.gethostname())
print (s.getsockname()[0])
client_socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
try:
        client_socket.connect(ADDR)
        client_socket.send("time_based_firewall" + "," + s.getsockname()[0])
except Exception as e:
        print("%s:%s" % ADDR)
        sys.exit()

client_socket.close()
s.close()

