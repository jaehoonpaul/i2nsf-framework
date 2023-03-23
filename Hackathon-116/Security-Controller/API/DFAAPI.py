import stack
import parsing

# DFA node for extractor
class DFAnode:
  def __init__(self, nodetype):
    self.nodetype = nodetype
    self.taglist = []
    self.pointlist = []
    self.index = -1

  def setinfo(self, index):
    self.index = index

  def connectNode(self, lowerNode, tag):
    self.pointlist.append(lowerNode)
    self.taglist.append('<'+tag+'>')
    lowerNode.pointlist.append(self)
    lowerNode.taglist.append('</'+tag+'>')

  def sendString(self, string_in):
    for i in range(len(self.taglist)):
      if string_in.startswith(self.taglist[i]):
        return [True, string_in[len(self.taglist[i]):], self.pointlist[i]]
    return [False]

  def extract(self, string_in, extractedlist):
    i = 0
    while string_in[i] != '<' or i >= len(string_in):
      i += 1
    if i < len(string_in):
      if string_in[0:i].isnumeric():
        extractedlist[self.index].append(int(string_in[0:i]))
      else:
        extractedlist[self.index].append(string_in[0:i])
      return string_in[i:]
    else:
      return ''


# DFA construction function
def dfa_construction(file_data_model):
  # recognize Consumer-Facing Interface Data Model
  fcfi = open(file_data_model, 'r')
  line = fcfi.readline()   # ignore first line
  field = ''
  extractedinfo = []
  index = 0

  # declare stack for automatic construction
  st = stack.Stack()
  node_accepter = DFAnode('accepter')
  st.push(node_accepter, False)

  # read all line
  id = 0
  while True:
    line = fcfi.readline()
    if not line:
      break

    # parsing line
    lineparsing = parsing.parsing(line,id)
    skip = lineparsing[0]
    level = lineparsing[1]
    field = lineparsing[2]
    isExtractor = lineparsing[3]

    # declare DFA node
    if isExtractor == True:
      dfanode = DFAnode('extractor')
      dfanode.setinfo(index)
      index += 1
      extractedinfo.append(lineparsing)
    else:
      dfanode = DFAnode('middle')

    # DFA node connection by investigating level
    while st.level() != level:
      st.pop()
    st.topnode().connectNode(dfanode, field)
    st.push(dfanode, skip)
    #('now field: '+field+', level: '+str(level))
    id+=1

  fcfi.close()
  #print('Complete to construct DFA for '+file_data_model)
  return [node_accepter, extractedinfo]


# extracting function
def extract_data(xml, node_accepter, extractedinfo):
  # call high-level policy
  string_policy = ''.join(xml.split())

  # declare list for extracting
  infolen = len(extractedinfo)
  extractedlist = []
  for i in range(infolen):
    extractedlist.append([])
  currentState = [True, string_policy, node_accepter]

  while True:
    currentState = currentState[2].sendString(currentState[1])
    if not currentState[0]:
      print('Wrong Grammar!')
      return (None,None)
    elif currentState[2].nodetype == 'accepter':
      #print('Success to extract:')
      break
    elif currentState[2].nodetype == 'extractor':
      remain = currentState[2].extract(currentState[1], extractedlist)
      if remain == '':
        print('Fail to extract!')
        break
      else:
        currentState[1] = remain
  #print("Extracted List: ", extractedlist)
  # # debug extracting
  # for i in range(infolen):
  #   if extractedlist[i]:
  #     print(str(i)+'\t'+extractedinfo[i][2]+': '+str(extractedlist[i]))

  return (extractedinfo, extractedlist)

# extracting function from a file
def extracting_data(file_high_level_policy, node_accepter, extractedinfo):
  # call high-level policy
  fi = open(file_high_level_policy, 'r')
  string_temp = fi.read()
  string_policy = ''.join(string_temp.split())
  fi.close()

  # declare list for extracting
  infolen = len(extractedinfo)
  extractedlist = []
  for i in range(infolen):
    extractedlist.append([])
  currentState = [True, string_policy, node_accepter]

  while True:
    currentState = currentState[2].sendString(currentState[1])
    if not currentState[0]:
      print('Wrong Grammar!')
      break
    elif currentState[2].nodetype == 'accepter':
      #print('Success to extract '+file_high_level_policy+':')
      break
    elif currentState[2].nodetype == 'extractor':
      remain = currentState[2].extract(currentState[1], extractedlist)
      if remain == '':
        print('Fail to extract!')
        break
      else:
        currentState[1] = remain
  #print("Extracted List: ", extractedlist)
  # debug extracting
  # for i in range(infolen):
  #   if extractedlist[i]:
  #     print(str(i)+'\t'+extractedinfo[i][2]+': '+str(extractedlist[i]))

  return (extractedinfo, extractedlist)


#consumer = dfa_construction('DataModel/cfi.txt')
#print(consumer[1])
#print(extracting_data('HighLevelPolicy/rule.txt',consumer[0],consumer[1]))

