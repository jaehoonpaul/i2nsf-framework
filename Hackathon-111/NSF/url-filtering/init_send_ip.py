#from socket import *
import socket
import sys
from time import ctime

HOST = '10.0.0.3' #EDIT to your DMS IP address
PORT = 55561
ADDR = (HOST,PORT)
s = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
s.connect(('115.145.178.170',0)) #EDIT to your Openstack IP address
print (socket.gethostname())
print (s.getsockname()[0])
client_socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
try:
        client_socket.connect(ADDR)
        client_socket.send("url_filtering" + "," + s.getsockname()[0])
except Exception as e:
        print("%s:%s" % ADDR)
        sys.exit()

client_socket.close()
s.close()

