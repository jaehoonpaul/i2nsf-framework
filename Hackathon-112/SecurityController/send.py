import os
import sys
import socket
import threading
import time

client_socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
HOST='10.0.0.17'
PORT=55570
ADDR=(HOST,PORT)
data='rule.txt'
try:
	client_socket.connect(ADDR)
	client_socket.send(data)
	print(data)
except Exception as e:
	print("%s:%s" % ADDR)
	sys.exit()
