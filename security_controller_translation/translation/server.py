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


print("111")
# construct DFA based Data Extractor
consumer = DFA.dfa_construction('DataModel/new_cfi_dm.txt')
consumer_dfa = consumer[0]
consumer_extractedinfo = consumer[1]

nsf_test = DFA.dfa_construction('DataModel/nfi_dm.txt')
nsf_test_dfa = nsf_test[0]
nsf_test_extractedinfo = nsf_test[1]

# construct CFG based Policy Generator
nsf_facing = CFG.cfg_construction('DataModel/nfi_dm.txt')
cfglist = nsf_facing[0]
nsf_requiredinfo = nsf_facing[1]


# construct Data Converter
dataconverter = converter.DataConverter(consumer_extractedinfo, nsf_requiredinfo)
dataconverter.initializeDB()

# connect with DMS
registration_interface = threading.Thread(target=socketAPI.openRegistrationInterface, args=('0.0.0.0', 55552, dataconverter))
tacker_interface = threading.Thread(target=socketAPI.receive_nsf_ip, args=('0.0.0.0', 55560))

#registration_interface = socketAPI.SocketThread(target=socketAPI.openRegistrationInterface, args=('127.0.0.1', 55552, dataconverter))
#tacker_interface = socketAPI.SocketThread(target=socketAPI.receive_nsf_ip, args=('10.0.0.7', 55560))

registration_interface.start()
tacker_interface.start()

# open server
server_socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
server_socket.bind(('0.0.0.0', 12345))
server_socket.listen(0)
print("222")

while True:
  print("Waiting for High level policy...")
  client_socket, addr = server_socket.accept()
  tag = client_socket.recv(4096)
  print("3333")
  if tag == b'1':
    response = os.popen("curl --ipv4 --http2 -k --cert-type PEM -E /home/ubuntu/works/jetconf/data/example-client.pem -X GET https://0.0.0.0:8443/restconf/data/ietf-i2nsf-cfi-policy:i2nsf-cfi").read()
    data = parser.json2xml(json.loads(response))
    fxml = open('policy.txt', 'w')
    fxml.write(data)
    fxml.close()
    consumer_extractedlist = DFA.extracting_data('policy.txt', consumer_dfa, consumer_extractedinfo)
    os.system("curl --ipv4 --http2 -k --cert-type PEM -E /home/ubuntu/works/jetconf/data/example-client.pem -X DELETE $POST_DATA https://0.0.0.0:8443/restconf/data/ietf-i2nsf-cfi-policy:i2nsf-cfi/policy")
    #nsf_extractedlist_1 = DFA.extracting_data('LowLevelPolicy/'+policyname+'1.txt', nsf_test_dfa, nsf_test_extractedinfo)
    #nsf_extractedlist_2 = DFA.extracting_data('LowLevelPolicy/'+policyname+'2.txt', nsf_test_dfa, nsf_test_extractedinfo)

    # convert data
    print('Convert data...')
    dataconverter.inputExtractedData(consumer_extractedlist)
    if dataconverter.convertData():
      print('Policy provisioning...')
      # policy provisioning
      dataconverter.constructDecisionTree()
      dataconverter.policyprovisioning(cfglist, '10.0.0.12', 55560)

