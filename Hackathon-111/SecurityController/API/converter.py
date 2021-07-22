import MySQLdb
import CFGAPI
import DecisionTree
import socketAPI
import os

class DataConverter:
  def matchData(self, src_index, dest_index):
    if self.extractedlist[src_index]:
      self.requiredlist[dest_index] = self.extractedlist[src_index]
      self.ismatched[src_index] = True
      self.xmlhead = '<?xml version="1.0" encoding="UTF-8"?>\n<hello xmlns="urn:ietf:params:xml:ns:netconf:base:1.0">\n<capabilities>\n<capability>urn:ietf:params:netconf:base:1.0</capability>\n</capabilities>\n</hello>\n]]>]]>\n<?xml version="1.0" encoding="UTF-8"?>\n<rpc message-id="1" xmlns="urn:ietf:params:xml:ns:netconf:base:1.0">\n<edit-config>\n <target>\n<running />\n</target>\n<config>\n<i2nsf-security-policy xmlns="urn:ietf:params:xml:ns:yang:ietf-i2nsf-policy-rule-for-nsf" xmlns:nc="urn:ietf:params:xml:ns:netconf:base:1.0">'
      self.xmltail = '</i2nsf-security-policy>\n</config>\n</edit-config>\n</rpc>\n]]>]]>\n<?xml version="1.0" encoding="UTF-8"?>\n<rpc message-id="2" xmlns="urn:ietf:params:xml:ns:netconf:base:1.0">\n<close-session />\n</rpc>\n]]>]]>'

  def setData(self, data, dest_index):
    if not self.requiredlist[dest_index]:
      self.requiredlist[dest_index] = data
    else:
      self.requiredlist[dest_index].append(data[0])

  def initializeDB(self):
    nsfdb = MySQLdb.connect(host="localhost", user="root", passwd="secu", db="nsfdb")
    nsfcur = nsfdb.cursor()

    nsfcur.execute("DROP TABLE nsftable")
    nsfcur.execute("CREATE TABLE nsftable (nname VARCHAR(255), processing VARCHAR(30), outbound VARCHAR(30), inbound VARCHAR(30), initiated VARCHAR(5))")
    """
    nsfcur.execute("INSERT INTO nsftable VALUES ('general_firewall', '1000,5000', '1000,5000', '1000,5000', 'False')")
    nsfcur.execute("INSERT INTO nsftable VALUES ('time_based_firewall', '1000,5000', '1000,5000', '1000,5000', 'False')")
    nsfcur.execute("INSERT INTO nsftable VALUES ('voip_volte_filter', '1000,5000', '1000,5000', '1000,5000', 'False')")
    nsfcur.execute("INSERT INTO nsftable VALUES ('web_filter', '1000,5000', '1000,5000', '1000,5000', 'False')")
    nsfcur.execute("INSERT INTO nsftable VALUES ('http_and_https_flood_mitigation', '1000,5000', '1000,5000', '1000,5000', 'False')")
    """

    nsfcur.execute("DROP TABLE capabilitytable")
    nsfcur.execute("CREATE TABLE capabilitytable (nname VARCHAR(255), cname VARCHAR(255))")
    """
    nsfcur.execute("INSERT INTO capabilitytable VALUES ('general_firewall', 'ipv4-capa')")
    nsfcur.execute("INSERT INTO capabilitytable VALUES ('general_firewall', 'tcp-capa')")
    nsfcur.execute("INSERT INTO capabilitytable VALUES ('general_firewall', 'ingress-action-capa')")
    nsfcur.execute("INSERT INTO capabilitytable VALUES ('general_firewall', 'egress-action-capa')")

    nsfcur.execute("INSERT INTO capabilitytable VALUES ('time_based_firewall', 'time-capabilities')")
    nsfcur.execute("INSERT INTO capabilitytable VALUES ('time_based_firewall', 'ipv4-capa')")
    nsfcur.execute("INSERT INTO capabilitytable VALUES ('time_based_firewall', 'ingress-action-capa')")
    nsfcur.execute("INSERT INTO capabilitytable VALUES ('time_based_firewall', 'egress-action-capa')")

    nsfcur.execute("INSERT INTO capabilitytable VALUES ('voip_volte_filter', 'voip-volte-capa')")
    nsfcur.execute("INSERT INTO capabilitytable VALUES ('voip_volte_filter', 'ingress-action-capa')")
    nsfcur.execute("INSERT INTO capabilitytable VALUES ('voip_volte_filter', 'egress-action-capa')")

    nsfcur.execute("INSERT INTO capabilitytable VALUES ('web_filter', 'http-capa')")
    nsfcur.execute("INSERT INTO capabilitytable VALUES ('web_filter', 'ingress-action-capa')")
    nsfcur.execute("INSERT INTO capabilitytable VALUES ('web_filter', 'egress-action-capa')")

    nsfcur.execute("INSERT INTO capabilitytable VALUES ('http_and_https_flood_mitigation', 'antiddos-capa')")
    nsfcur.execute("INSERT INTO capabilitytable VALUES ('http_and_https_flood_mitigation', 'ingress-action-capa')")
    nsfcur.execute("INSERT INTO capabilitytable VALUES ('http_and_https_flood_mitigation', 'egress-action-capa')")
    """

    nsfcur.execute("DROP TABLE fieldtable")
    nsfcur.execute("CREATE TABLE fieldtable (cname VARCHAR(255), fieldname VARCHAR(255))")
    """
    nsfcur.execute("INSERT INTO fieldtable VALUES ('ipv4-capa', 'range-ipv4-address')")
    nsfcur.execute("INSERT INTO fieldtable VALUES ('ipv4-capa', 'exact-ipv4-address')")
    nsfcur.execute("INSERT INTO fieldtable VALUES ('ipv4-capa', 'ipv4-protocol')")
    nsfcur.execute("INSERT INTO fieldtable VALUES ('tcp-capa', 'exact-tcp-port-num')")
    nsfcur.execute("INSERT INTO fieldtable VALUES ('tcp-capa', 'range-tcp-port-num')")
    nsfcur.execute("INSERT INTO fieldtable VALUES ('ingress-capa', 'alert')")
    nsfcur.execute("INSERT INTO fieldtable VALUES ('ingress-capa', 'drop')")
    nsfcur.execute("INSERT INTO fieldtable VALUES ('ingress-capa', 'pass')")
    nsfcur.execute("INSERT INTO fieldtable VALUES ('egress-capa', 'alert')")
    nsfcur.execute("INSERT INTO fieldtable VALUES ('egress-capa', 'drop')")
    nsfcur.execute("INSERT INTO fieldtable VALUES ('egress-capa', 'pass')")
    nsfcur.execute("INSERT INTO fieldtable VALUES ('time-capabilities', 'absolute-time')")
    nsfcur.execute("INSERT INTO fieldtable VALUES ('time-capabilities', 'periodic-time')")
    nsfcur.execute("INSERT INTO fieldtable VALUES ('voip-volte-capa', 'voice-id')")
    nsfcur.execute("INSERT INTO fieldtable VALUES ('http-capa', 'url')")
    nsfcur.execute("INSERT INTO fieldtable VALUES ('antiddos-capa', 'http-flood-action')")
    nsfcur.execute("INSERT INTO fieldtable VALUES ('antiddos-capa', 'https-flood-action')")
    nsfcur.execute("INSERT INTO fieldtable VALUES ('antiddos-capa', 'mitigation')")
    """

    nsfcur.execute("DROP TABLE endpointtable")
    nsfcur.execute("CREATE TABLE endpointtable (ename VARCHAR(255), id INT(1), data VARCHAR(255))")
    #nsfcur.execute("INSERT INTO endpointtable (ename, id, data) VALUES ('sns-websites', 114, 'facebook,instagram')")
    nsfcur.execute("INSERT INTO endpointtable (ename, id, data) VALUES ('sns-websites', 123, 'url-filtering')")
    nsfcur.execute("INSERT INTO endpointtable (ename, id, data) VALUES ('ddos-attacker', 150, '10.0.0.37')")

    #nsfcur.execute("INSERT INTO endpointtable (ename, id, data) VALUES ('employees', 43, '10.0.0.3')")
    #nsfcur.execute("INSERT INTO endpointtable (ename, id, data) VALUES ('employees', 44, '10.0.0.40')")
    nsfcur.execute("INSERT INTO endpointtable (ename, id, data) VALUES ('malicious-id', 84, '5060,5061')")
    nsfcur.execute("INSERT INTO endpointtable (ename, id, data) VALUES ('malicious-id', 115, '11111@voip.black.com,22222@voip.black.com')")
    nsfcur.execute("INSERT INTO endpointtable (ename, id, data) VALUES ('malicious-id', 123, 'voip-volte')")
    nsfcur.execute("INSERT INTO endpointtable (ename, id, data) VALUES ('webservers', 46, '221.159.112.95')")
    nsfcur.execute("INSERT INTO endpointtable (ename, id, data) VALUES ('webservers', 84, '80,443')")
    nsfcur.execute("INSERT INTO endpointtable (ename, id, data) VALUES ('webservers', 124, 'http_and_https_flood')")
    #nsfcur.execute("INSERT INTO endpointtable (ename, id, data) VALUES ('webservers', 124, 'https_flood')")

    nsfdb.commit()

    nsfcur.close()
    nsfdb.close()
    print('NSF Database is constructed')

  def registerNSF(self, data):
    nsfdb = MySQLdb.connect(host="localhost", user="root", passwd="secu", db="nsfdb")
    nsfcur = nsfdb.cursor()
    nsf_name, processing, outbound, inbound = '', '', '', ''
    print(data)
    linelist = data.split('\n')
    for line in linelist:
      if line == '':  continue
      linetemp = line.split(': ')
      if len(linetemp) < 2:  continue
      capa = linetemp[0]
      field = linetemp[1]
      #print(capa)
      #print(field)
      if 'nsf-name' in capa:  nsf_name = field
      elif 'processing' in capa:  processing = field
      elif 'Bandwidth Outbound' in capa:  outbound = field
      elif 'Bandwidth Inbound' in capa:  inbound = field
      else:
        nsfcur.execute("INSERT INTO capabilitytable VALUES ('"+nsf_name+"', '"+capa+"')")
        fieldlist = field.split(',')
        for field in fieldlist:
          nsfcur.execute("INSERT INTO fieldtable VALUES ('"+capa+"', '"+field+"')")
    nsfcur.execute("INSERT INTO nsftable VALUES ('"+nsf_name+"', '"+processing+"', '"+outbound+"', '"+inbound+"', 'False')")
    nsfdb.commit()
    nsfcur.close()
    nsfdb.close()
    print(nsf_name+": NSF and Capabilities are Registered")

  # convert data
  def convertData(self):
    self.matchData(0, 0)   # match policy-name
    self.matchData(1, 4)   # match rule-name
    self.matchData(15, 118) # match packet-per-second
    self.matchData(20, 120) # match ingress-action
    self.matchData(20, 121) # match egress-action

    # match start-time
    if self.extractedlist[7]:
      self.requiredlist[8].append(self.extractedlist[7][0]+':00Z')

    # match end-time
    if self.extractedlist[8]:
      self.requiredlist[9].append(self.extractedlist[8][0]+':00Z')

    # register endpoint information to NSF Database
    # need to fill

    # match via NSF Database
    nsfdb = MySQLdb.connect(host="localhost", user="root", passwd="secu", db="nsfdb")
    nsfcur = nsfdb.cursor()
    infolen = len(self.extractedinfo)
    for i in range(infolen):
      if self.ismatched[i]:  continue
      if self.extractedlist[i]:
        nsfcur.execute("SELECT id, data FROM endpointtable WHERE ename='"+self.extractedlist[i][0]+"'")
        rows = nsfcur.fetchall()
        for ptr in rows:
          self.setData(ptr[1].split(','), ptr[0])
    nsfcur.close()
    nsfdb.close()

  def constructDecisionTree(self):
    # initialize
    nsfdb = MySQLdb.connect(host="localhost", user="root", passwd="secu", db="nsfdb")
    nsfcur = nsfdb.cursor()
    self.capabilitylist = []
    self.learning_input = []
    self.learning_output = []
    self.nsflist = []
    self.nsf_capability = []

    # find all registered capabilities
    nsfcur.execute("SELECT DISTINCT cname FROM capabilitytable")
    rows = nsfcur.fetchall()
    rowlen = len(rows)
    for ptr in rows:
      self.capabilitylist.append(ptr[0])
      self.learning_output.append([])

    # construct database for decision tree
    nsfcur.execute("SELECT DISTINCT nname FROM nsftable")
    rows = nsfcur.fetchall()
    for ptr in rows:
      self.nsflist.append(ptr[0])

    for nsf in self.nsflist:
      temp_capalist = []
      nsfcur.execute("SELECT cname FROM capabilitytable WHERE nname='"+nsf+"'")
      rows = nsfcur.fetchall()
      for ptr in rows:
        temp_capalist.append(ptr[0])
      self.nsf_capability.append(temp_capalist)

    """
    learning_temp = []
    for i in range(len(self.capabilitylist)):
      learning_temp.append(False)
    self.learning_input.append(learning_temp)
    for i in range(len(self.nsf_capability)):
      self.learning_output[i].append(False)
    """

    for x in range(len(self.nsf_capability)):
      learning_temp = []
      for i in range(len(self.capabilitylist)):
        if self.capabilitylist[i] in self.nsf_capability[x]:
          learning_temp.append(True)
        else:
          learning_temp.append(False)
      self.learning_input.append(learning_temp)
      for y in range(len(self.nsf_capability)):
        self.learning_output[y].append(x==y)

    for i in range(len(self.nsf_capability)):
      capa_temp = []
      for j in range(len(self.nsf_capability[i])):
        capa_temp.append(self.nsf_capability[i][j])
      for j in range(len(self.nsf_capability)):
        if i!=j:
          for k in range(len(self.nsf_capability[j])):
            if self.nsf_capability[j][k] in capa_temp:
              capa_temp.remove(self.nsf_capability[j][k])
      learning_temp = []
      for j in range(len(self.capabilitylist)):
        if self.capabilitylist[j] in capa_temp:
          learning_temp.append(True)
        else:
          learning_temp.append(False)
      self.learning_input.append(learning_temp)
      for y in range(len(self.nsf_capability)):
        self.learning_output[y].append((i==y and len(capa_temp)>0))

    # construct Decision Tree
    self.dtlist = []
    for i in range(len(self.nsf_capability)):
      self.dtlist.append(DecisionTree.DecisionTreeNode(self.learning_input, self.learning_output[i], 0.005, self.capabilitylist))
    nsfcur.close()
    nsfdb.close()

  def policyprovisioning(self, cfglist, requestIP, requestPORT):
    nsfdb = MySQLdb.connect(host="localhost", user="root", passwd="secu", db="nsfdb")
    nsfcur = nsfdb.cursor()
    capalen = len(self.capabilitylist)
    infolen = len(self.requiredinfo)
    nsflen = len(self.nsf_capability)

    # vector for investigating policy provisioning
    test_input = []

    # vector generation
    for i in range(capalen):
      isExist = False
      for j in range(infolen):
        if self.requiredlist[j]:
          if cfglist[j].isAncestor(self.capabilitylist[i]):
            isExist = True
            break
          else:
            nsfcur.execute("SELECT fieldname FROM fieldtable WHERE cname='"+self.capabilitylist[i]+"'")
            rows = nsfcur.fetchall()
            for ptr in rows:
              if cfglist[j].isAncestor(ptr[0]):
                isExist = True
                break
            if isExist:  break
      test_input.append(isExist)

    # endpoint information: exception
    if not self.requiredlist[0]:
      test_input = []
      for i in range(capalen):
        test_input.append(False)

    # policy provisioning
    selectednsfstring = ''
    for i in range(nsflen):
      isSelected = self.dtlist[i].running(test_input)
      if isSelected:
        nsfcur.execute("SELECT initiated FROM nsftable WHERE nname='"+self.nsflist[i]+"'")
        rows = nsfcur.fetchall()
        if rows[0][0] == 'False':
          # initiate
          print('Initiate NSF: '+self.nsflist[i]+'\n')
          selectednsfstring += (self.nsflist[i]+',')
          #socketAPI.request_nsf('10.0.0.12', 55560, self.nsflist[i])
          nsfcur.execute("UPDATE nsftable SET initiated = 'True' WHERE nname='"+self.nsflist[i]+"'")
          nsfdb.commit()

        # provide data for required capabilities
        requiredlist = []
        for j in range(infolen):
          isExist = False
          for capa in self.nsf_capability[i]:
            if cfglist[j].isAncestor(capa):
              isExist = True
              break
            else:
              nsfcur.execute("SELECT fieldname FROM fieldtable WHERE cname='"+capa+"'")
              rows = nsfcur.fetchall()
              for ptr in rows:
                if cfglist[j].isAncestor(ptr[0]):
                  isExist = True
                  break
              if isExist:  break
          if isExist:
            if (j == 120 or j == 121) and ('firewall' in self.nsflist[i]):  requiredlist.append(['pass'])  # action exception for firewall
            elif (j == 125) and ('firewall' in self.nsflist[i]):  requiredlist.append(['drop'])  # action exception for firewall
            elif (j == 50 or j == 51) and (not self.requiredlist[115]):  requiredlist.append([])  # dest-ip exception for firewall
            elif (j == 43 or j == 44) and (not self.requiredlist[114]):  requiredlist.append([])  # src-ip exception for firewall
            elif (j == 123 or j == 124) and (not 'firewall' in self.nsflist[i]):  requiredlist.append([])  # advanced-security exception for filter
            else:  requiredlist.append(self.requiredlist[j])
          else:
            if j == 0 or j == 4:  requiredlist.append(self.requiredlist[j])  # name exception for all NSFs
            elif (j == 123 or j == 124) and ('firewall' in self.nsflist[i]):  requiredlist.append(self.requiredlist[j])  # advanced-security exception for firewall
            elif (j == 118) and (not 'firewall' in self.nsflist[i]):  requiredlist.append(self.requiredlist[j])  # packet-per-second exception for firewall
            else:  requiredlist.append([])

        # generate and provide low-level policy to NSF
        print('Low-level policy for '+self.nsflist[i])
        print(self.xmlhead+CFGAPI.generating_policy(cfglist, self.requiredinfo, requiredlist).rstrip()+self.xmltail)
        fo = open('./LowLevelPolicy/'+self.nsflist[i]+'.txt', 'w')
        fo.write(self.xmlhead+CFGAPI.generating_policy(cfglist, self.requiredinfo, requiredlist).rstrip()+self.xmltail)
        fo.close()
        fo = open('./LowLevelPolicy/'+self.nsflist[i]+'.xml', 'w')
        fo.write(self.xmlhead+CFGAPI.generating_policy(cfglist, self.requiredinfo, requiredlist).rstrip()+self.xmltail)
        fo.close()
      print('')

    if selectednsfstring != '':
      socketAPI.request_nsf(requestIP, requestPORT, selectednsfstring[:-1])
      #socketAPI.request_nsf('10.0.0.12', 55560, selectednsfstring[:-1])

    nsfcur.close()
    nsfdb.close()

  def __init__(self, extractedinfo, requiredinfo):
    self.extractedinfo = extractedinfo
    self.requiredinfo = requiredinfo

  def inputExtractedData(self, extractedlist):
    self.extractedlist = extractedlist
    self.requiredlist = []
    self.ismatched = []
    infolen = len(self.requiredinfo)
    for i in range(infolen):
      self.requiredlist.append([])
    infolen = len(self.extractedinfo)
    for i in range(infolen):
      self.ismatched.append(False)

