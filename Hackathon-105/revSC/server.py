import API.DFAAPI as DFA
import API.CFGAPI as CFG
import API.converter as converter
import API.socketAPI as socketAPI
import threading
import os
import json
import socket
import API.parsing as parser
import MySQLdb

# construct DFA based Data Extractor
consumer = DFA.dfa_construction('DataModel/new_cfi_dm.txt')
consumer_dfa = consumer[0]
consumer_extractedinfo = consumer[1]

# construct CFG based Policy Generator
nsf_facing = CFG.cfg_construction('DataModel/nfi_dm.txt')
cfglist = nsf_facing[0]
nsf_requiredinfo = nsf_facing[1]

# construct Data Converter
dataconverter = converter.DataConverter(consumer_extractedinfo, nsf_requiredinfo)
dataconverter.initializeDB()

# connect with DMS

registration_interface = threading.Thread(target=socketAPI.openRegistrationInterface, args=('0.0.0.0', 55552, dataconverter))
tacker_interface = threading.Thread(target=socketAPI.receive_nsf_ip, args=('0.0.0.0', 55570))
registration_interface.start()
tacker_interface.start()


# open server
server_socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
server_socket.bind(('0.0.0.0', 12345))
server_socket.listen(0)

while True:
  print("Waiting for High level policy...")
  client_socket, addr = server_socket.accept()
  temp = client_socket.recv(4096)
  client_socket, addr = server_socket.accept()
  tag = client_socket.recv(4096)
 # print(temp)
 # print(tag)
  if tag == b'1':

    response = temp.decode('utf-8')
#    client_socket2 = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
#    client_socket2.connect(('127.0.0.1', 55560))
#    client_socket2.send("OK\n")

    #response = os.popen("curl --ipv4 --http2 -k --cert-type PEM -E /home/deploy/jetconf/data/example-client.pem -X GET https://0.0.0.0:8443/restconf/data/ietf-i2nsf-cfi-policy:i2nsf-cfi").read()
 #   print(type(response))
 #   print("test2")

#    os.system("curl --ipv4 --http2 -k --cert-type PEM -E /home/deploy/jetconf/data/example-client.pem -X GET $POST_DATA https://0.0.0.0:8443/restconf/data/ietf-i2nsf-cfi-policy:i2nsf-cfi:policy")


    data = parser.json2xml(json.loads(response))
    fxml = open('policy.txt', 'w')
    fxml.write(data)
    fxml.close()
    consumer_extractedlist = DFA.extracting_data('policy.txt', consumer_dfa, consumer_extractedinfo)
#    os.system("curl --ipv4 --http2 -k --cert-type PEM -E /home/deploy/jetconf/data/example-client.pem -X DELETE $POST_DATA https://0.0.0.0:8443/restconf/data/ietf-i2nsf-cfi-policy:i2nsf-cfi/policy")


    # convert data
    print('Convert data...')
    dataconverter.inputExtractedData(consumer_extractedlist)
    if dataconverter.convertData():
      print('Policy provisioning...')
      # policy provisioning
      dataconverter.constructDecisionTree()
      dataconverter.policyprovisioning(cfglist, '127.0.0.1', 55570)

