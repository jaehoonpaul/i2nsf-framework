import stack
import parsing

# Priority Queue for automatic CFG construction
class PriorityQueue:
  def __init__(self):
    self.datalist = []
    self.size = 0

  def swap(self, index1, index2):
    temp = self.datalist[index1]
    self.datalist[index1] = self.datalist[index2]
    self.datalist[index2] = temp

  def enqueue(self, data):
    self.datalist.append(data)
    self.size += 1
    if self.size == 1:  return

    index = self.size-1
    while index >= 0:
      pindex = int((index+1)/2)-1
      if pindex < 0:  break
      if self.datalist[index].level <= self.datalist[pindex].level:  break
      self.swap(index, pindex)
      index = pindex

  def dequeue(self):
    if self.size == 0:
      print('Priority Queue is empty!')
    self.size -= 1
    self.swap(0, self.size)
    result = self.datalist.pop()
    index = 0
    right = 0
    left = 0

    while True:
      right = (index+1)*2
      left = right-1
      if right > self.size:
        break
      if right == self.size:
        if self.datalist[left].level > self.datalist[index].level:
          self.swap(index, left)
        break
      if self.datalist[left].level > self.datalist[index].level:
        if self.datalist[right].level > self.datalist[left].level:
          self.swap(index, right)
          index = right
        else:
          self.swap(index, left)
          index = left
      elif self.datalist[right].level > self.datalist[index].level:
        self.swap(index, right)
        index = right
      else:
        break
    return result

  def top(self):
    return self.datalist[0]

  def isExist(self, data):
    for i in range(self.size):
      if self.datalist[i] == data:
        return True
    return False

# Grammar for generator
class TextfreeGrammar:
  def __init__(self, grammartype, level):
    self.grammartype = grammartype
    self.level = level
    self.datalist = []
    self.index = -1
    self.order = -1
    self.name = ''

  def setorder(self, order):
    self.order = order

  def setinfo(self, index):
    self.index = index

  def setParent(self, parent):
    self.parent = parent

  def setDatalist(self, datalist):
    self.datalist = datalist

  def pushDatalist(self, data):
    for i in range(len(self.datalist)):
      if self.datalist[i].order > data.order:
        self.datalist.insert(i, data)
        return
    self.datalist.append(data)

  def settag(self, starttag, endtag):
    self.starttag = starttag
    self.endtag = endtag

  def setname(self, name):
    self.name = name

  def isAncestor(self, name):
    if (name in self.name) or (self.name in name):
      return True
    if not self.parent:
      return False
    return self.parent.isAncestor(name)

  def translate(self, level):
    string_out = ''
    if self.grammartype == 'structure':
      for i in range(level):
        string_out += '  '
      string_out += self.starttag+'\n'
      for i in range(len(self.datalist)):
        string_out += self.datalist[i].translate(level+1)+'\n'
      for i in range(level):
        string_out += '  '
      string_out += self.endtag
    else:
      for i in range(len(self.datalist)):
        for j in range(level):
          string_out += '  '
        string_out += self.starttag
        string_out += self.datalist[i]
        string_out += self.endtag
        if i!=len(self.datalist)-1:
          string_out += '\n'
    return string_out


# CFG construction function
def cfg_construction(file_data_model):
  # recognize NSF-Facing Interface Data Model
  fnfi = open(file_data_model, 'r')
  line = fnfi.readline()   # ignore first line
  field = ''
  requiredinfo = []
  contentcfglist = []
  order = 0

  # declare stack for Data Model tree recognition
  st = stack.Stack()
  st.push('', False)

  # read all line
  while True:
    line = fnfi.readline()
    if not line:
      break

    # parsing line
    lineparsing = parsing.parsing(line)
    skip = lineparsing[0]
    level = lineparsing[1]
    field = lineparsing[2]
    isContent = lineparsing[3]

    # declare CFG node
    if isContent:
      cfgnode = TextfreeGrammar('content', level)
      cfgnode.setinfo(len(requiredinfo))
      requiredinfo.append(field)
      contentcfglist.append(cfgnode)
    else:
      cfgnode = TextfreeGrammar('structure', level)
    cfgnode.settag('<'+field+'>', '</'+field+'>')
    cfgnode.setname(field)
    cfgnode.setorder(order)
    order += 1

    # CFG node connection by investigating level
    while st.level() != level:
      st.pop()
    cfgnode.setParent(st.topnode())
    st.push(cfgnode, skip)
    #print('now field: '+field+', level: '+str(st.level()))

  fnfi.close()

  print('Complete to construct CFG for '+file_data_model+'\n')
  #print(requiredinfo)
  return [contentcfglist, requiredinfo]


# function for generating low-level policy
def generating_policy(contentcfglist, requiredinfo, requiredlist):
  infolen = len(requiredinfo)

  # declare priority queue for generating low-level policy
  pqueue = PriorityQueue()

  # push all content node which contains data
  for i in range(infolen):
    if requiredlist[i]:
      pqueue.enqueue(contentcfglist[i])
  
  while pqueue.size > 1:
    cfgnode = pqueue.dequeue()
    # print(cfgnode.level)
    if cfgnode.grammartype == 'content':
      cfgnode.setDatalist(requiredlist[cfgnode.index])
    cfgnode.parent.pushDatalist(cfgnode)
    if not pqueue.isExist(cfgnode.parent):  pqueue.enqueue(cfgnode.parent)

  # generate high-level policy
  finalnode = pqueue.top()
  result = finalnode.translate(0)

  # CFG node initiation
  for i in range(infolen):
    temp = contentcfglist[i]
    while temp != finalnode:
      temp.setDatalist([])
      temp = temp.parent
      if not temp.datalist:  break
  finalnode.setDatalist([])

  return result+'\n\n'
