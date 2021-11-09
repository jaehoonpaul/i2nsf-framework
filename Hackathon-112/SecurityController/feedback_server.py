#!/usr/bin/python
#-*- coding:utf-8 -*-
import os
import MySQLdb
import API.DFAAPI as DFA
import API.CFGAPI as CFG
import API.converter as converter
import API.socketAPI as socketAPI
import socket
import threading

# construct DFA based Data Extractor
consumer = DFA.dfa_construction('DataModel/cfi_dm.txt')
consumer_dfa = consumer[0]
consumer_extractedinfo = consumer[1]

# construct CFG based Policy Generator
nsf_facing = CFG.cfg_construction('DataModel/nfi_dm.txt')
cfglist = nsf_facing[0]
nsf_requiredinfo = nsf_facing[1]

# construct Data Converter
dataconverter = converter.DataConverter(consumer_extractedinfo, nsf_requiredinfo)
#print('Data Converter constructed')
dataconverter.initializeDB()
#print('NSF Database is constructed')

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
        server_socket.bind(('10.0.0.17', 55580))
        server_socket.listen(5)
        client_socket, addr = server_socket.accept()
        data = client_socket.recv(1024)
        #policyname = raw_input()
        # extract data
	f = open('HighLevelPolicy/feedback.txt','w')
	f.write(data)
	f.close()
        consumer_extractedlist = DFA.extracting_data('HighLevelPolicy/feedback.txt', consumer_dfa, consumer_extractedinfo)
        print(consumer_extractedlist[11][0])
        nsfdb = MySQLdb.connect(host="localhost", user="root", passwd="secu", db="nsfdb")
        nsfcur = nsfdb.cursor()
        nsfcur.execute("SELECT data FROM endpointtable WHERE ename='"+consumer_extractedlist[11][0]+"'")
        rows = nsfcur.fetchone()
	print(rows[0])
	translated = """<?xml version="1.0" encoding="UTF-8"?>
<hello xmlns="urn:ietf:params:xml:ns:netconf:base:1.0">
<capabilities>
<capability>urn:ietf:params:netconf:base:1.0</capability>
</capabilities>
</hello>
]]>]]>
<?xml version="1.0" encoding="UTF-8"?>
<rpc message-id="1" xmlns="urn:ietf:params:xml:ns:netconf:base:1.0">
<edit-config>
 <target>
<running />
</target>
<config>
<i2nsf-security-policy xmlns="urn:ietf:params:xml:ns:yang:ietf-i2nsf-policy-rule-for-nsf" xmlns:nc="urn:ietf:params:xml:ns:netconf:base:1.0"><system-policy>
  <system-policy-name>security_policy_for_ddos_attack</system-policy-name>
  <rules>
    <rule-name>ddos_attack_rule</rule-name>
    <condition-clause-container>
      <packet-security-ipv4-condition>
        <pkt-sec-ipv4-src>
          <ipv4-address>
            <ipv4>10.0.0.37</ipv4>
          </ipv4-address>
        </pkt-sec-ipv4-src>
      </packet-security-ipv4-condition>
    </condition-clause-container>
    <action-clause-container>
      <packet-action>
        <ingress-action>drop</ingress-action>
        <egress-action>drop</egress-action>
      </packet-action>
    </action-clause-container>
  </rules>
</system-policy></i2nsf-security-policy>
</config>
</edit-config>
</rpc>
]]>]]>
<?xml version="1.0" encoding="UTF-8"?>
<rpc message-id="2" xmlns="urn:ietf:params:xml:ns:netconf:base:1.0">
<close-session />
</rpc>
]]>]]>
"""
        print (translated)
	print ("SENDING NEW CONFIGURATION")
	os.system("sudo /home/ubuntu/confd-6.6/bin/netconf-console --host 10.0.0.13 LowLevelPolicy/feedback_ddos.xml")

        # convert data
#        print('Convert data...')
#        dataconverter.inputExtractedData(consumer_extractedlist)
#        dataconverter.convertData()
#
#        print('Policy provisioning...')
        # policy provisioning
#        dataconverter.constructDecisionTree()
#        dataconverter.policyprovisioning(cfglist, '10.0.0.3', 55560) #EDIT WITH YOUR DMS IP

registration_interface.exit()
registration_interface.join()
tacker_interface.stop()
tacker_interface.join()
