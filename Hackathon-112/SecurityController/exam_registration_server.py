import socket
import threading



def receive_capa():
	while True:
		server_socket = socket.socket()
		server_socket.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
		server_socket.bind(('127.0.0.1', 55552))
		server_socket.listen(5)
		client_socket, addr = server_socket.accept()
		data = client_socket.recv(1024)
		print(data)
#		server_socker.close()

threading._start_new_thread(receive_capa, ())

while True:
	pass

