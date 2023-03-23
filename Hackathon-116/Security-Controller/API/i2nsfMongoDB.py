# -*- coding: utf-8 -*-
"""
Created on Thu Jun  2 15:35:20 2022

@author: Patrick
"""

import pymongo
from regex import R
import mapper
from pprint import pprint

    
# Register url-group to MongoDB
# Data Model: {"name": string, "url": [string]}
def insertURLGroup(data):
    try:
        client = pymongo.MongoClient("mongodb://172.17.20.250:27017/")
        db = client["endpoint"]
        col = db["url"]
        
        res = col.insert_one(data)
        return res
    except pymongo.errors.DuplicateKeyError:
        print("Duplicate Key for ",data["name"])


def getURLGroup(key):
    client = pymongo.MongoClient("mongodb://172.17.20.250:27017/")
    db = client["endpoint"]
    col = db["url"]
    
    query = {"name":key}
    res = col.find_one(query)
    return res

# Register user-group to MongoDB
# Data Model: {"name": string, "mac-address": [yang:mac-address], "range-ipv4-address": {"start": ipv4-address,"end": ipv4-address}, "range-ipv6-address": {"start": ipv6-address, "end": ipv6-address}}
def insertUserGroup(data):
    try:
        client = pymongo.MongoClient("mongodb://172.17.20.250:27017/")
        db = client["endpoint"]
        col = db["user"]
        
        res = col.insert_one(data)
        return res
    except pymongo.errors.DuplicateKeyError:
        print("Duplicate Key for ",data["name"])

def getUserGroup(key):
    client = pymongo.MongoClient("mongodb://172.17.20.250:27017/")
    db = client["endpoint"]
    col = db["user"]
    
    query = {"name":key}
    res = col.find_one(query)
    return res

# Register location-group to MongoDB
# Data Model: {"name": string, "geo-ipv4-address": {}, "ipv4-address": {"start": ipv4-address,"end": ipv4-address}, "ipv6-address": {"start": ipv6-address,"end": ipv6-address}}
def insertLocationGroup(data):
    try:
        client = pymongo.MongoClient("mongodb://172.17.20.250:27017/")
        db = client["endpoint"]
        col = db["location"]
        
        res = col.insert_one(data)
        return res
    except pymongo.errors.DuplicateKeyError:
        print("Duplicate Key for ",data["name"])

def getLocationGroup(country,region,city):
    client = pymongo.MongoClient("mongodb://172.17.20.250:27017/")
    db = client["endpoint"]
    col = db["location"]
    
    query = {"country":country,"region":region,"city":city}
    res = col.find_one(query)
    return res

# Register Attributes Mapping
def insertAttributesMap(cfiTree,nfiTree):
    try:
        mapResult = mapper.mapAttributes(cfiTree,nfiTree)
        client = pymongo.MongoClient("mongodb://172.17.20.250:27017/")
        db = client["endpoint"]
        col = db["mapping"]

        for key,values in mapResult.items():
            mapDict = {}
            mapDict["cfiPath"]=key.path()
            mapDict["cfiID"]=key.id
            mapDict["map"]=[]
            for val in values:
                mapDict["map"].append({"nfiId":val.id,"nfiPath":val.path()})

            res = col.insert_one(mapDict)
        return res 
    except pymongo.errors.DuplicateKeyError:
        print("Duplicate Key for ",mapDict["cfiID"])

def getAttributesMap(cfiID):
    client = pymongo.MongoClient("mongodb://172.17.20.250:27017/")
    db = client["endpoint"]
    col = db["mapping"]
    
    query = {"cfiID":cfiID}
    res = col.find_one(query)
    return res

#Register NSF Capability, parameter in Python Diction / JSON, Pattern:
# {'nsf-name': string, "nsf-capability-info": Follow-Registration-Data-Model, "nsf-access-info": Follow-Registration-Data-Model}
def insertCapability(capability):
    client = pymongo.MongoClient("mongodb://172.17.20.250:27017/")
    db = client["nsfDB"]
    col = db["capabilities"]
    
    res = col.insert_one(capability)
    return res

def getAllCapability(query={}):
    client = pymongo.MongoClient("mongodb://172.17.20.250:27017/")
    db = client["nsfDB"]
    col = db["capabilities"]
    
    res = col.find(query)
    return res

#FINDING ITEM IN A DICTIONARY
def findItem(obj,key):
    if key in obj:
        return obj[key]
    for k, val in obj.items():
        if isinstance(val,dict):
            item = findItem(val, key)
            if item is not None:
                return item

def findCapability(filter,query={}):
    print(filter)
    NSF = []
    for x in getAllCapability(query):
        for key, val in filter.items():
            if findItem(x,key):
                if val in findItem(x,key):
                    NSF.append(x)
    return NSF

#CAPABILITY MAPPING
def insertCapabilityMapping(capabilityMapping):
    client = pymongo.MongoClient("mongodb://172.17.20.250:27017/")
    db = client["mapping"]
    col = db["capabilityMapping"]

    res = col.insert_many(capabilityMapping)
    return res

def getCapabilityMapping(val):
    client = pymongo.MongoClient("mongodb://172.17.20.250:27017/")
    db = client["mapping"]
    col = db["capabilityMapping"]

    query = {"path":val}
    res = col.find_one(query)
    return res

#CAPABILITY PATH
def insertCapabilityPath(capabilityPath):
    client = pymongo.MongoClient("mongodb://172.17.20.250:27017/")
    db = client["mapping"]
    col = db["capabilityPath"]

    res = col.insert_many(capabilityPath)
    return res

def getCapabilityPath(val):
    client = pymongo.MongoClient("mongodb://172.17.20.250:27017/")
    db = client["mapping"]
    col = db["capabilityPath"]

    query = {"key":val}
    res = col.find_one(query)
    return res


#NEXT HEADER
def insertNextHeader(nextHeader):
    client = pymongo.MongoClient("mongodb://172.17.20.250:27017/")
    db = client["mapping"]
    col = db["nextHeader"]

    res = col.insert_many(nextHeader)
    return res

def getNextHeader(val):
    client = pymongo.MongoClient("mongodb://172.17.20.250:27017/")
    db = client["mapping"]
    col = db["nextHeader"]

    query = {"keyword":val}
    res = col.find_one(query)
    return res


#ICMP MESSAGE
def insertICMPMessage(icmpMessage):
    client = pymongo.MongoClient("mongodb://172.17.20.250:27017/")
    db = client["mapping"]
    col = db["icmpMessage"]

    res = col.insert_many(icmpMessage)
    return res

def getICMPMessage(val):
    client = pymongo.MongoClient("mongodb://172.17.20.250:27017/")
    db = client["mapping"]
    col = db["icmpMessage"]

    query = {"keyword":val}
    res = col.find_one(query)
    return res


