import os
import sys
import socket
import threading
import time

client_socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
HOST='10.0.0.17'
PORT=55580
ADDR=(HOST,PORT)
data=open('feedback.xml','r')
try:
        client_socket.connect(ADDR)
        client_socket.send(data.read())
        print(data.read())
except Exception as e:
        print("%s:%s" % ADDR)
        sys.exit()
