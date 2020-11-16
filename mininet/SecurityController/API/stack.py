# Stack for tree recognition
class Stack:
  def __init__(self):
    self.datalist = []
    self.skipflag = []
    self.top = -1

  def push(self, data, skip):
    self.top += 1
    self.datalist.append(data)
    self.skipflag.append(skip)

  def topnode(self):
    index = self.top
    while self.skipflag[index] == True:
      index -= 1
    return self.datalist[index]

  def pop(self):
    self.top -= 1
    self.skipflag.pop()
    return self.datalist.pop()

  def level(self):
    return self.top
