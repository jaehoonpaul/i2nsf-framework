# -*- coding: utf-8 -*-
"""
Created on Thu May 26 15:57:46 2022

@author: Patrick
"""

import re
import operator
import math
from collections import Counter
from zss import simple_distance, Node #Zhang-Shasha Algorithm for Tree Edit Distance Calculation

#Python Class for an attribute in a YANG Data Model. Each node of YANG Data Model is assigned in yangdm python class.
class yangdm:
    def __init__ (self, tag, id, isLeaf, level, skip):
        self.tag = tag         # The TAG of the node in the YANG Data Model
        self.id = id           # The ID of the node in the YANG Data Model --> The ID should be assigned sequentially from top to bottom of the YANG DM
        self.isLeaf = isLeaf   # True if the node is a Leaf, False if it is not a Leaf
        self.level = level     # Level of the node YANG DM (Level 0 = Root, Level 1 = child of root)
        self.skip = skip       # True if it is skippable, e.g., Match-Case in YANG Data Model
        self.parent = None     # Parent of the node
        self.child = []        # Child of the node. If the node is a leaf, it is empty, i.e., []
        
    def setParent(self,parent): # For setting the parent of the node
        self.parent = parent
    
    def printParent(self):   # Printing the parent
        if self.parent is not None:
            print("Parent : {}".format(self.parent.tag))
        
    def printDM(self):       # Printing the node details
        print("Tag    : {}".format(self.tag))
        print("ID     : {}".format(self.id))
        print("isLeaf : {}".format(self.isLeaf))
        print("Level  : {}".format(self.level))
        print("Skip   : {}".format(self.skip))
        yangdm.printParent(self)
        print("Child  : {}".format(self.child))
    
    def setChild(self,child): # Setting the child of the node
        self.child.append(child)
    
    def setLeafID(self,leafID):
        self.leafID = leafID
        
    def path(self): #Get the path of the attribute
        if self.parent is None:
            return "/{}".format(self.tag)
        else:
            return "{}/{}".format(self.parent.path(),self.tag)
        
#Parsing from YANG Tree Data Model into Python Class yangdm. Reading each line of the YANG Tree Data Model
def parsing(line,id):
    linelen = len(line)
    skip = False
    
    start = 0
    while line[start] == ' ' or line[start] == '|':
        start += 1
    level = int((start-2)/3)
    
    if line[start+3] == 'r':
        start += 6
    else:
        start += 4
    end = start
    
    if line[start] == '(':
        for i in range(start, linelen):
            if line[i] == ')':
                end = i
                break
        tag = line[start:end+1]
        start = end
        skip = True
    else:
        for i in range(start, linelen):
            if line[i] =='*' or line[i] =='?' or line[i] ==' 'or line[i] == '\n':
                end = i
                break
        tag = line[start:end]
        if line[end] == '*' or line[end] == '?':
            start = end + 1
        else:
            start = end
            
    isLeaf=True
    end = start
    while line[end] ==' ':
        end += 1
    if line[end] == '[' or line[end]=='\n':
        isLeaf = False
    data = yangdm(tag=tag,id=id,level=level,isLeaf=isLeaf, skip=skip)
    return data

def text_to_vector(text):
    WORD = re.compile(r"\w+")
    words = WORD.findall(text)
    return Counter(words)

def get_cosine(vector1, vector2):
    vec1 = text_to_vector(vector1)
    vec2 = text_to_vector(vector2)
    intersection = set(vec1.keys()) & set(vec2.keys())
    numerator = sum([vec1[x] * vec2[x] for x in intersection])

    sum1 = sum([vec1[x] ** 2 for x in list(vec1.keys())])
    sum2 = sum([vec2[x] ** 2 for x in list(vec2.keys())])

    denominator = math.sqrt(sum1) * math.sqrt(sum2)
    if not denominator:
        return 1 * int(max(len(vector1),len(vector2)))
    else:
        return int(len(vector1) * (1- float(numerator) / denominator))
    
def weird_dist(A, B):
    """
       Available to use for calculating Distance:
       1. strdist(A,B)
       2. seqMatch(A,B)
       3. get_cosine(A,B)
    """
    return get_cosine(A, B) 

class WeirdNode(object):

    def __init__(self, label):
        self.my_label = label
        self.my_children = list()

    @staticmethod
    def get_children(node):
        return node.my_children

    @staticmethod
    def get_label(node):
        return node.my_label

    def addkid(self, node, before=False):
        if before:  self.my_children.insert(0, node)
        else:   self.my_children.append(node)
        return self
    
    
def mapAttributes(cfiTree,nfiTree):
    # READ CFI / High-Level YANG Data Model tree and parse it into python yang dm class
    with open(cfiTree,'r') as f:
        next(f)
        id = 0
        cfiFull = [] #Full data model
        for line in f:
            cfiFull.append(parsing(line,id))
            id += 1
            
    for i in range(len(cfiFull)-1,-1,-1):
        for j in range(i-1,-1,-1):
            if cfiFull[i].level > cfiFull[j].level:
                cfiFull[i].setParent(cfiFull[j])
                break
    
    for i in range(len(cfiFull)-1,-1,-1):
        try:
            while cfiFull[i].parent.skip:
                cfiFull[i].setParent(cfiFull[i].parent.parent)
        except:
            pass
    
    for i in range(len(cfiFull)):
        for j in range(len(cfiFull)):
            try:
                if cfiFull[i].id == cfiFull[j].parent.id and not cfiFull[j].skip:
                    cfiFull[i].setChild(cfiFull[j])
            except:
                pass
                    
    cfiLeaf = [] #For only the leaf data model
    
    leafID=0
    for x in cfiFull:
        if x.isLeaf and not x.skip:
            x.setLeafID(leafID)
            cfiLeaf.append(x)
            leafID+=1
            
    cfiNonLeaf = []
    for x in cfiFull:
        if not x.isLeaf and not x.skip:
            cfiNonLeaf.append(x)
            
    # READ NFI / High-Level YANG Data Model tree and parse it into python yang dm class
    with open(nfiTree,'r') as f:
        next(f)
        id = 0
        nfiFull = [] #Full data model
        for line in f:
            nfiFull.append(parsing(line,id))
            id += 1
            
    for i in range(len(nfiFull)-1,-1,-1):
        for j in range(i-1,-1,-1):
            if nfiFull[i].level > nfiFull[j].level:
                nfiFull[i].setParent(nfiFull[j])
                break
    
    for i in range(len(nfiFull)-1,-1,-1):
        try:
            while nfiFull[i].parent.skip:
                nfiFull[i].setParent(nfiFull[i].parent.parent)
        except:
            pass
    
    for i in range(len(nfiFull)):
        for j in range(len(nfiFull)):
            try:
                if nfiFull[i].id == nfiFull[j].parent.id and not nfiFull[j].skip:
                    nfiFull[i].setChild(nfiFull[j])
            except:
                pass
                    
    nfiLeaf = [] #For only the leaf data model
    nfiLeafID = 0
    for x in nfiFull:
        if x.isLeaf and not x.skip:
            x.setLeafID(nfiLeafID)
            nfiLeaf.append(x)
            nfiLeafID+=1
            
    nfiNonLeaf = []
    for x in nfiFull:
        if not x.isLeaf and not x.skip:
            nfiNonLeaf.append(x)

    #Separating the YANG Tree into 1 branch
    #Edge Contraction also used here --> If the parent and the leaf has similar label, the edge is deleted, then the parent and the child is unified
    def ctree (leafNode): #leafNode is a yangdm Python Class
        Nodes = list()
        Nodes.append(leafNode)
        parentNode = leafNode.parent
        while parentNode is not None:
            try:
                Nodes.append(parentNode)
            except AttributeError:
                pass
            leafNode = leafNode.parent
            parentNode = parentNode.parent
        return Nodes
    
    cfiM = list()
    nfiM = list()
    cfiNL = list()
    nfiNL = list()
    for i in range(len(cfiLeaf)):
        cfiM.append(ctree(cfiLeaf[i]))
        
    for j in range(len(nfiLeaf)):
        nfiM.append(ctree(nfiLeaf[j]))
        
    for i in range(len(cfiNonLeaf)):
        cfiNL.append(ctree(cfiNonLeaf[i]))
        
    for j in range(len(nfiNonLeaf)):
        nfiNL.append(ctree(nfiNonLeaf[j]))
    
    def getChild(parent):
        allChild = []
        for child in parent.child:
            allChild.append(child)
            if child.child:
                leaf = getChild(child)
                for l in leaf:
                    allChild.append(l)
        return allChild
    
    #MAPPING THE NON LEAF
    parentMap = {}
    for w in range(len(cfiNonLeaf)):
        distance = list()
        for x in reversed(cfiNL[w]):
            if x.parent is None:
                A = WeirdNode(x.tag)
            else:
                A.addkid(WeirdNode(x.tag))
        if cfiNonLeaf[w].parent is not None:
            minDistances = {}
            #print(cfiNonLeaf[w].tag)
            for x in parentMap[cfiNonLeaf[w].parent]:
                #print(cfiNonLeaf[i].tag, cfiNonLeaf[i].parent.tag, x.tag)
                nfiPair = getChild(x)
                nfiPair.insert(0,nfiPair[0].parent)
                distance=[]
    
                for j in range(len(nfiPair)):
                    F=''
                    for y in reversed(ctree(nfiPair[j])):
                        F+=y.tag+'/'
                        if y.parent is None:
                            B = WeirdNode(y.tag)
                        else:
                            B.addkid(WeirdNode(y.tag))
                    if nfiPair[j].isLeaf:
                        distance.append(10000)
                    else:
                        distance.append(int(simple_distance(A, B, WeirdNode.get_children, WeirdNode.get_label, weird_dist)))
                index = [i for i, x in enumerate(distance) if x == min(distance)]
                for i in index:
                    minDistances[nfiPair[i]] = min(distance)
            min_val = min(minDistances.values())
            index = [k for k, x in minDistances.items() if x == min_val]
            parentMap[cfiNonLeaf[w]] = [k for k in index]
        else:
            for j in range(len(nfiNonLeaf)):
                for y in reversed(nfiNL[j]):
                    if y.parent is None:
                        B = WeirdNode(y.tag)
                    else:
                        B.addkid(WeirdNode(y.tag))
                distance.append(int(simple_distance(A, B, WeirdNode.get_children, WeirdNode.get_label, weird_dist)))
    
            index, value = min(enumerate(distance), key=operator.itemgetter(1))
            index = [i for i, x in enumerate(distance) if x == min(distance)]
            parentMap[cfiNonLeaf[w]]= [nfiNonLeaf[k] for k in index]
    
    finalMap = {}
    res = {}
    for w in range(len(cfiLeaf)):
        minDistances = {}
        current = cfiLeaf[w]
    
        cfiM = ctree(current)
        nfiparents = parentMap[current.parent]
        for x in reversed(cfiM):
            if x.parent is None:
                A = WeirdNode(x.tag)
            else:
                A.addkid(WeirdNode(x.tag))
        for nfiparent in (nfiparents):
            distance=[]
            nfiPair = getChild(nfiparent)
    
    
            for j in range(len(nfiPair)):
                nfiM = ctree(nfiPair[j])
                for x in reversed(nfiM):
                    if x.parent is None:
                        B = WeirdNode(x.tag)
                    else:
                        B.addkid(WeirdNode(x.tag))
    
                distance.append(int(simple_distance(A, B, WeirdNode.get_children, WeirdNode.get_label, weird_dist)))
    
            index = [i for i, x in enumerate(distance) if x == min(distance)]
            for i in index:
                minDistances[nfiPair[i]] = min(distance)
    
        min_val = min(minDistances.values())
        index = [k for k, x in minDistances.items() if x == min_val]
        finalMap[current] = [i for i in index]
        for i in index:
            if current in res:
                res[current].append(i)
            else:
                res[current] = [i]
    return res