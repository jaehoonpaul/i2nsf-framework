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
        ip=None
        if (i2nsfMongoDB.getUserGroup(value)):
            userGroup = i2nsfMongoDB.getUserGroup(value)
            if userGroup['mac-address']:
                #print(lowAttr['map'][0]['nfiPath'])
                lowData[lowAttr['map'][0]['nfiPath']] = userGroup['mac-address']
            if userGroup['range-ipv4-address']['start'] and userGroup['range-ipv4-address']['end']:
                ip = "ipv4"
                lowData[lowAttr['map'][2]['nfiPath']] = userGroup['range-ipv4-address']['start'] + " " + userGroup['range-ipv4-address']['end']
            if userGroup['range-ipv6-address']['start'] and userGroup['range-ipv6-address']['end']:
                ip = "ipv6"
                lowData[lowAttr['map'][4]['nfiPath']] = userGroup['range-ipv6-address']['start'] + " " + userGroup['range-ipv6-address']['end']
        elif (keys == 60 or keys == 61 or keys == 62):
            locationGroup = i2nsfMongoDB.getLocationGroup(highData[60],highData[61],highData[62])
            if locationGroup["geo-ipv4-address"]["start"] and locationGroup["geo-ipv4-address"]["end"]:
                lowData["/i2nsf-security-policy/rules/condition/ipv4/source-ipv4-range"] = locationGroup["geo-ipv4-address"]["start"] + " " +locationGroup["geo-ipv4-address"]["end"]
            if locationGroup["geo-ipv6-address"]["start"] and locationGroup["geo-ipv6-address"]["end"]:
                lowData["/i2nsf-security-policy/rules/condition/ipv6/source-ipv6-range"] = locationGroup["geo-ipv4-address"]["start"] + " " +locationGroup["geo-ipv4-address"]["end"]
        elif (keys == 64 or keys == 65 or keys == 66):
            locationGroup = i2nsfMongoDB.getLocationGroup(highData[64],highData[65],highData[66])
            if locationGroup["geo-ipv4-address"]["start"] and locationGroup["geo-ipv4-address"]["end"]:
                lowData["/i2nsf-security-policy/rules/condition/ipv4/destination-ipv4-range"] = locationGroup["geo-ipv4-address"]["start"] + " " +locationGroup["geo-ipv4-address"]["end"]
            if locationGroup["geo-ipv6-address"]["start"] and locationGroup["geo-ipv6-address"]["end"]:
                lowData["/i2nsf-security-policy/rules/condition/ipv6/destination-ipv6-range"] = locationGroup["geo-ipv4-address"]["start"] + " " +locationGroup["geo-ipv4-address"]["end"]
        elif (i2nsfMongoDB.getURLGroup(value)):
            urlData = i2nsfMongoDB.getURLGroup(value)
            lowData[lowAttr['map'][1]['nfiPath']] = urlData['urls']
        elif (lowAttr['cfiPath'] == "/i2nsf-cfi-policy/rules/condition/firewall/transport-layer-protocol"):
            val = i2nsfMongoDB.getNextHeader(value)["protocol-number"]
            if not ip:
                pass
            elif ip == "ipv6":
                lowData[lowAttr['map'][1]['nfiPath']] = int(val)
            else:
                lowData[lowAttr['map'][0]['nfiPath']] = int(val)
        elif (lowAttr['cfiPath'] == "/i2nsf-cfi-policy/rules/condition/firewall/icmp/message"):
            if isinstance(value,str):
                value=[value]
            if '/i2nsf-security-policy/rules/condition/ipv6/destination-ipv6-range' in lowData or '/i2nsf-security-policy/rules/condition/ipv6/source-ipv6-range' in lowData:
                lowData[lowAttr['map'][0]['nfiPath']] = "icmpv6"
                for val in value:
                    lowData[lowAttr['map'][1]['nfiPath']] = int(i2nsfMongoDB.getICMPMessage(val)["icmpv6"]["type"])
                    lowData[lowAttr['map'][2]['nfiPath']] = int(i2nsfMongoDB.getICMPMessage(val)["icmpv6"]["code"])
            else: #Default is ICMPv4
                lowData[lowAttr['map'][0]['nfiPath']] = "icmpv4"
                for val in value:
                    lowData[lowAttr['map'][1]['nfiPath']] = int(i2nsfMongoDB.getICMPMessage(val)["icmpv4"]["type"])
                    lowData[lowAttr['map'][2]['nfiPath']] = int(i2nsfMongoDB.getICMPMessage(val)["icmpv4"]["code"])
        elif (lowAttr['cfiPath']=="/i2nsf-cfi-policy/rules/condition/firewall/range-port-number/start"):
            start = value
            end = highData[17]
            print(highData[17])
            if (14 not in highData or highData[14] =="tcp"):
                if 12 in highData:
                    lowData[lowAttr['map'][0]['nfiPath']] = str(start) + " " + str(end)
                if 13 in highData:
                    lowData[lowAttr['map'][1]['nfiPath']] = str(start) + " " + str(end)
                if (12 not in highData and 13 not in highData):
                    lowData[lowAttr['map'][0]['nfiPath']] = str(start) + " " + str(end)
                    lowData[lowAttr['map'][1]['nfiPath']] = str(start) + " " + str(end)
            else:
                if highData[14] == "udp":
                    if 12 in highData:
                        lowData[lowAttr['map'][2]['nfiPath']] = str(start) + " " + str(end)
                    if 13 in highData:
                        lowData[lowAttr['map'][3]['nfiPath']] = str(start) + " " + str(end)
                    if (12 not in highData and 13 not in highData):
                        lowData[lowAttr['map'][2]['nfiPath']] = str(start) + " " + str(end)
                        lowData[lowAttr['map'][3]['nfiPath']] = str(start) + " " + str(end)
                elif highData[14] == "sctp":
                    if 12 in highData:
                        lowData[lowAttr['map'][4]['nfiPath']] = str(start) + " " + str(end)
                    if 13 in highData:
                        lowData[lowAttr['map'][5]['nfiPath']] = str(start) + " " + str(end)
                    if (12 not in highData and 13 not in highData):
                        lowData[lowAttr['map'][4]['nfiPath']] = str(start) + " " + str(end)
                        lowData[lowAttr['map'][5]['nfiPath']] = str(start) + " " + str(end)
                elif highData[14] == "dccp":
                    if 12 in highData:
                        lowData[lowAttr['map'][6]['nfiPath']] = str(start) + " " + str(end)
                    if 13 in highData:
                        lowData[lowAttr['map'][7]['nfiPath']] = str(start) + " " + str(end)
                    if (12 not in highData and 13 not in highData):
                        lowData[lowAttr['map'][6]['nfiPath']] = str(start) + " " + str(end)
                        lowData[lowAttr['map'][7]['nfiPath']] = str(start) + " " + str(end)
        elif (lowAttr['cfiPath']=="/i2nsf-cfi-policy/rules/condition/firewall/range-port-number/end"):
            pass
        else:
            lowData[lowAttr['map'][0]['nfiPath']] = value
    print(lowData)
    return(lowData)

# mydict = {"ipv4-capability": "source-address"}
# findCapability(mydict)
# highData = {1: 'security_policy_for_blocking_sns', 5: 'block_access_to_sns_during_office_hours', 12: 'employees', 30: 'sns-websites', 64: 'drop'}

# print(convertMongo(highData))