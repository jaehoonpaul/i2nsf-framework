# -*- coding: utf-8 -*-
"""
Created on Mon May 30 16:09:31 2022

@author: Patrick
"""

import i2nsfDB
import i2nsfMongoDB
from collections import OrderedDict

def convert(highData,db):
    lowData = OrderedDict()
    for keys,value in highData.items():
        lowAttr = i2nsfDB.mapAttr(keys,db)
        if (i2nsfDB.getUserData(value,db)):
            name, mac, start_ipv4, end_ipv4, start_ipv6, end_ipv6 = i2nsfDB.getUserData(value,db)
            if mac:
                lowData[lowAttr[0][1]] = mac
            elif start_ipv4 and end_ipv4:
                lowData[lowAttr[2][1]] = start_ipv4 + " " + end_ipv4
            elif start_ipv6 and end_ipv6:
                lowData[lowAttr[4][1]] = start_ipv6 + " " + end_ipv6
        elif (i2nsfDB.getURLData(value,db)):
            urlData = i2nsfDB.getURLData(value,db)
            lowData[lowAttr[2][1]] = []
            if urlData:
                for data in urlData:
                    lowData[lowAttr[2][1]].append(data[1])
        else:
            lowData[lowAttr[0][1]] = value
    return(lowData)

def convertMongo(highData):
    lowData = OrderedDict()
    for keys,value in highData.items():
        lowAttr = i2nsfMongoDB.getAttributesMap(keys)
        if (i2nsfMongoDB.getUserGroup(value)):
            userGroup = i2nsfMongoDB.getUserGroup(value)
            if userGroup['mac-address']:
                lowData[lowAttr['map'][0]['nfiPath']] = userGroup['mac-address']
            elif userGroup['range-ipv4-address']['start'] and userGroup['range-ipv4-address']['end']:
                lowData[lowAttr['map'][2]['nfiPath']] = userGroup['range-ipv4-address']['start'] + " " + userGroup['range-ipv4-address']['end']
            elif userGroup['range-ipv6-address']['start'] and userGroup['range-ipv6-address']['end']:
                lowData[lowAttr['map'][4]['nfiPath']] = userGroup['range-ipv6-address']['start'] + " " + userGroup['range-ipv6-address']['end']
        elif (i2nsfMongoDB.getURLGroup(value)):
            urlData = i2nsfMongoDB.getURLGroup(value)
            lowData[lowAttr['map'][2]['nfiPath']] = urlData['urls']
        elif (lowAttr['map'][0]['nfiPath'] == "/i2nsf-security-policy/rules/condition/ipv4/protocol"):
            val = i2nsfMongoDB.getNextHeader(value)["protocol-number"]
            lowData[lowAttr['map'][0]['nfiPath']] = int(val)
        elif (lowAttr['cfiPath'] == "/i2nsf-cfi-policy/rules/condition/firewall/icmp/message"):
            if '/i2nsf-security-policy/rules/condition/ipv6/destination-ipv6-range' in lowData or '/i2nsf-security-policy/rules/condition/ipv6/source-ipv6-range' in lowData:
                lowData[lowAttr['map'][1]['nfiPath']] = "icmpv6"
                lowData[lowAttr['map'][2]['nfiPath']] = int(i2nsfMongoDB.getICMPMessage(value)["icmpv6"]["type"])
                lowData[lowAttr['map'][3]['nfiPath']] = int(i2nsfMongoDB.getICMPMessage(value)["icmpv6"]["code"])
            else: #Default is ICMPv4
                lowData[lowAttr['map'][1]['nfiPath']] = "icmpv4"
                lowData[lowAttr['map'][2]['nfiPath']] = int(i2nsfMongoDB.getICMPMessage(value)["icmpv4"]["type"])
                lowData[lowAttr['map'][3]['nfiPath']] = int(i2nsfMongoDB.getICMPMessage(value)["icmpv4"]["code"])
        else:
            lowData[lowAttr['map'][0]['nfiPath']] = value
    return(lowData)

# mydict = {"ipv4-capability": "source-address"}
# findCapability(mydict)
# highData = {1: 'security_policy_for_blocking_sns', 5: 'block_access_to_sns_during_office_hours', 12: 'employees', 30: 'sns-websites', 64: 'drop'}

# print(convertMongo(highData))