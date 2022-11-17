#!/usr/bin/python
#-*- coding:utf-8 -*-

import DFAAPI as DFA
import CFGAPI as CFG
import converter as converter
import socketAPI as socketAPI
import socket
import threading
import Levenshtein as lev
from fuzzywuzzy import fuzz

# construct DFA based Data Extractor
consumer = DFA.dfa_construction('DataModel/cfi.txt')
consumer_dfa = consumer[0]
consumer_extractedinfo = consumer[1]

# construct CFG based Policy Generator
nsf_facing = CFG.cfg_construction('DataModel/nfi.txt')
cfglist = nsf_facing[0]
nsf_requiredinfo = nsf_facing[1]

# construct Data Converter
dataconverter = converter.DataConverter(consumer_extractedinfo, nsf_requiredinfo)

consumer_extractedlist = DFA.extracting_data('HighLevelPolicy/rule.txt', consumer_dfa, consumer_extractedinfo)
dataconverter.inputExtractedData(consumer_extractedlist)
# print(len(consumer[1]))
#print(nsf_facing[1])
a = consumer[1][12]
dis=[]
for nfi in nsf_facing[1]:
    dis.append(fuzz.ratio(a,nfi))
    
# print(a)
# print(nsf_facing[1][dis.index(max(dis))])

mapping = {
    0 : 0,
    26 : 119
}

for i in mapping:
    dataconverter.matchData(i,mapping.get(i))
print(dataconverter.requiredlist)
#print(nsf_requiredinfo)

lowpol = CFG.generating_policy(cfglist, dataconverter.requiredinfo, dataconverter.requiredlist)
print(lowpol)

"""
print(len(dataconverter.extractedlist))
print(dataconverter.extractedlist)


mapping = {0:0,1:4,11:6,19:5}

for i in mapping:
    dataconverter.matchData(i,mapping.get(i))
    

print(len(dataconverter.requiredlist))
print(dataconverter.requiredlist)
print(dataconverter.xmlhead+CFG.generating_policy(cfglist, dataconverter.requiredinfo, dataconverter.requiredlist).rstrip()+dataconverter.xmltail)
"""


#print('Data Converter constructed')
#dataconverter.initializeDB()
#print('NSF Database is constructed')
"""
# connect with DMS
registration_interface = threading.Thread(target=socketAPI.openRegistrationInterface, args=('10.0.0.17', 55552, dataconverter)) #EDIT WITH YOUR SECURITY_CONTROLLER IP
tacker_interface = threading.Thread(target=socketAPI.receive_nsf_ip, args=('10.0.0.17', 55560)) #EDIT WITH YOUR SECURITY_CONTROLLER IP

#registration_interface = socketAPI.SocketThread(target=socketAPI.openRegistrationInterface, args=('127.0.0.1', 55552, dataconverter))
#tacker_interface = socketAPI.SocketThread(target=socketAPI.receive_nsf_ip, args=('10.0.0.7', 55560))

registration_interface.start()
tacker_interface.start()

while True:
	server_socket = socket.socket()
	server_socket.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
	server_socket.bind(('10.0.0.17', 55570))
	server_socket.listen(5)
	client_socket, addr = server_socket.accept()
	data = client_socket.recv(1024)
	#policyname = raw_input()
	# extract data
	consumer_extractedlist = DFA.extracting_data('HighLevelPolicy/'+data, consumer_dfa, consumer_extractedinfo)

	# convert data
	print('Convert data...')
	dataconverter.inputExtractedData(consumer_extractedlist)
	dataconverter.convertData()

	print('Policy provisioning...')
	# policy provisioning
	dataconverter.constructDecisionTree()
	dataconverter.policyprovisioning(cfglist, '10.0.0.3', 55560) #EDIT WITH YOUR DMS IP

registration_interface.exit()
registration_interface.join()
tacker_interface.stop()
tacker_interface.join()
"""