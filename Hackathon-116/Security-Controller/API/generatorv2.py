# -*- coding: utf-8 -*-
"""
Created on Mon May 30 17:25:17 2022

@author: Patrick
"""

import DFAAPI
import MySQLdb
from convert import convert, convertMongo
from bindingNFI4 import ietf_i2nsf_nsf_facing_interface
#from bindingCFI import ietf_i2nsf_cfi_policy
from pybind.pyangbind.lib.serialise import pybindIETFXMLEncoder
from rbindings.ietf_i2nsf_registration_interface_rpc.nsf_capability_registration.input import input
from rbindings.ietf_i2nsf_registration_interface_rpc.nsf_capability_registration.output import output
import logging
import re
import i2nsfMongoDB
import query
from collections import OrderedDict

from datetime import datetime

from collections import OrderedDict
import i2nsfMongoDB as  mongo
import json
import itertools
import pulp

#ConvertedData = OrderedDict([('/i2nsf-security-policy/name', 'security_policy_for_blocking_sns'), ('/i2nsf-security-policy/rules/name', 'block_access_to_sns_during_office_hours'), ('/i2nsf-security-policy/rules/condition/ipv4/source-ipv4-range', '192.0.2.0 192.0.2.255'), ('/i2nsf-security-policy/rules/condition/ipv4/protocol', 6), ('/i2nsf-security-policy/rules/condition/url-category/user-defined', ['facebook', 'instagram']), ('/i2nsf-security-policy/rules/action/packet-action/ingress-action', 'drop')])

def greedy(U,S):
    X = U
    
    C = []
    while X:
        cost = {}
        for Si,Pi in S.items():
            add = [s for s in Pi["s"] if s in X]
            try:
                cost[Si]=Pi["p"]/len(add)
            except:
                pass

        Selected = min(cost, key=cost.get)

        X = [x for x in X if x not in S[Selected]["s"]]

        C.append(Selected)
    return C

def combination(U,S):
    stuff = [s for s in S.keys()]
    res = {}
    for L in range(1,len(stuff) + 1):
        for subset in itertools.combinations(stuff, L):
            X = U
            P = 0
            for set in subset:
                X = [x for x in X if x not in S[set]["s"]]
                P = P + S[set]["p"]
            if not X:
                res[subset]=P
    minimum = min(res, key=res.get)
    return minimum

def linear_programming(U, S):
    # Create a binary variable for each set
    x = {i: pulp.LpVariable(f"x_{i}", cat="Binary") for i in S}
    # Create the linear programming problem
    lp_problem = pulp.LpProblem("Set_Cover_Problem", pulp.LpMinimize)

    # Add the objective function
    lp_problem += pulp.lpSum(S[i]["p"] * x[i] for i in S)
    
    # Add the constraints
    for j in U:
        lp_problem += pulp.lpSum(x[i] for i in S if j in S[i]["s"]) >= 1

    # Solve the problem
    lp_problem.solve(pulp.PULP_CBC_CMD(msg=False))

    # Return the selected sets
    result = [i for i in S if x[i].value() == 1] 
    #print("LP Cost: ",pulp.value(lp_problem.objective))
    return result

def findMap(key,cap):
    for x in cap:
        if key in x:
            return(x[key])

def coverSetNSF(convertedData):
    #start = datetime.now()
    Universe = []
    res = {}
    icmp = ""
    with open("capabilityMappingv2.json") as f:
        capDict = json.load(f)

    for key,value in convertedData.items():
        capMap = ""

        if key in capDict:
            capMap = capDict[key]

        if key == "/i2nsf-security-policy/rules/condition/icmp/version" and value =="icmpv6":
            icmp = "icmpv6"
        elif key == "/i2nsf-security-policy/rules/condition/icmp/version" and value == "icmpv4":
            icmp = "icmpv4"
        res[key]={}

        if capMap:
            if (icmp == "icmpv4" and "icmpv6-capability" in capMap):
                del capMap["icmpv6-capability"]
            elif (icmp == "icmpv6" and "icmpv4-capability" in capMap):
                del capMap["icmpv4-capability"]
            for capKey in capMap:
                if type(capMap[capKey]) is list:
                    Universe.append({capKey:value})
                    res[key]["capMap"] = {capKey:value}
                    res[key]["value"] = value
                else:
                    Universe.append(capMap)
                    res[key]["capMap"] = capMap
                    res[key]["value"] = value
        else:
            res[key]["value"] = value

    i=1
    Subset={}
    for u in Universe:
        
        NSF = mongo.findCapability(u,query={})
        if NSF:
            for x in NSF:
                if x["nsf-name"] in Subset:
                    Subset[x["nsf-name"]]["s"].append(u)
                else:
                    Subset[x["nsf-name"]]={}
                    Subset[x["nsf-name"]]["s"]=[]
                    Subset[x["nsf-name"]]["p"]=10
                    Subset[x["nsf-name"]]["s"].append(u)
                    i+=1
        else:
            #Query to DMS to get the NSF
            key = list(u.keys())[0]
            capabilityPath = mongo.getCapabilityPath(key)
            rpc_input = input()
            rpc_input._unset_query_nsf_capability()
            data = {capabilityPath['path']:u[key]}
            generateQuery(rpc_input,data)
            capabilityQuery = pybindIETFXMLEncoder.serialise(rpc_input).replace("input","nsf-capability-registration")
            queryResult = query.query(capabilityQuery)
            if queryResult == "NSF Not Found":
                return f"Cannot find NSF with {u}"
            else:
                if isinstance(queryResult,list):
                    for x in queryResult:
                        Subset[x["nsf-name"]]={}
                        Subset[x["nsf-name"]]["s"]=[]
                        Subset[x["nsf-name"]]["p"]=10
                        Subset[x["nsf-name"]]["s"].append(u)
                        i+=1
                else:
                    x = queryResult
                    Subset[x["nsf-name"]]={}
                    Subset[x["nsf-name"]]["s"]=[]
                    Subset[x["nsf-name"]]["p"]=10
                    Subset[x["nsf-name"]]["s"].append(u)
                    i+=1


    #chosenC = combination(Universe,Subset)
    #chosenG = greedy(Universe,Subset)
    chosen = linear_programming(Universe,Subset)
    # print("chosenC: ",chosenC)
    # print("chosenG: ",chosenG)
    # print("chosenL: ",chosenL)
    
    # chosen = chosenC

    # if set(chosenC) == set(chosenG):
    #     greedyOptimal = 1
    # else: greedyOptimal = 0

    # if set(chosenC) == set(chosenL):
    #     linearOptimal = 1
    # else: linearOptimal = 0

    result = OrderedDict()

    for k,v in res.items():
        for ch in chosen:
            if ("capMap" in v and v["capMap"] in Subset[ch]["s"]):
                if ("/i2nsf-security-policy/rules/action" in k and len(res)>1):
                    #print(k)
                    for i in range(len(chosen)-1):
                        result[chosen[i]]["/i2nsf-security-policy/rules/action/advanced-action/content-security-control"] = chosen[i+1]
                    result[chosen[-1]][k] = convertedData[k]
                else:
                    if ch not in result:
                        result[ch]={}
                    result[ch][k]=v["value"]
                break
            
            elif "capMap" not in v:
                if ch not in result:
                    result[ch]={}
                result[ch][k]=v["value"]

    #end = datetime.now()
    #time = end - start
    return result#, time)

def generateQuery(nfi,data):
    for path,value in data.items():
        #print(path,value)
        currentNode = nfi
        splitPath = path.replace('-','_').split("/")
        #print(splitPath)
        splitPath.pop(0) # Remove the first element of the as it has no value
        for k in range(len(splitPath)):
            if getattr(currentNode,splitPath[k])._is_leaf or getattr(currentNode,splitPath[k])._yang_type == "identityref":
                setattr(currentNode,splitPath[k],value)
                break
            currentNode = getattr(currentNode,splitPath[k])
            if currentNode._yang_type == "list":
                if k == len(splitPath)-1:
                    currentNode.add(value)
                elif (len(currentNode._keyval.split()) == 1 and splitPath[k+1] == currentNode._keyval):
                    currentNode.add(value)
                    break
                else:
                    splitNode = currentNode._keyval.split()
                    for split in splitNode:
                        keyPath = re.sub("\[.+?\]",'',"/{}/{}".format('/'.join(currentNode._path()),
                                                split.replace("_","-")))
                        currentNode = currentNode[data[keyPath]]

def generate(nfi,provisioning):
    res = {}
    nfi._unset_i2nsf_security_policy()
    if isinstance(provisioning,str):
        return provisioning
    for nsf,lowData in provisioning.items():
        if nsf is None:
            return "Error, NSF with the necessarry capability not found"
        for path,value in lowData.items():
            currentNode = nfi
            splitPath = path.replace('-','_').split("/")
            splitPath.pop(0) # Remove the first element as it has no value
            for k in range(len(splitPath)):
                if getattr(currentNode,splitPath[k])._is_leaf or getattr(currentNode,splitPath[k])._yang_type == "identityref":
                    setattr(currentNode,splitPath[k],value)
                    break
                elif getattr(currentNode,splitPath[k])._pybind_generated_by == 'TypedListType':
                    if isinstance(value,list):
                        for val in value:
                            getattr(currentNode,splitPath[k]).append(val)
                    else:
                        getattr(currentNode,splitPath[k]).append(value)
                currentNode = getattr(currentNode,splitPath[k])
                if currentNode._yang_type == "list":
                    if k == len(splitPath)-1:
                        currentNode.add(value)
                    elif splitPath[k+1] == currentNode._keyval:
                        currentNode.add(value)
                        break
                    else:
                        splitNode = currentNode._keyval.split()
                        for split in splitNode:
                            keyPath = re.sub("\[.+?\]",'',"/{}/{}".format('/'.join(currentNode._path()),
                                                    split.replace("_","-")))
                            currentNode = currentNode[lowData[keyPath]]


        #print(nsf,":")
        result = pybindIETFXMLEncoder.serialise(nfi)
        #print(result)
        res[nsf] = result
        nfi._unset_i2nsf_security_policy()
    return res


def gen(xml):
    consumer = DFAAPI.dfa_construction('DataModel/cfi_dm.txt')
    resInfo,resData = DFAAPI.extract_data(xml,consumer[0],consumer[1])
    if (not resInfo and not resData):
        return {"Error": "Grammar Error"}
    highData = {}
    for x in range(len(resInfo)):
        if resData[x]:
            #print("{id:<3}{attribute:<10}: {value}".format(id= resInfo[x][4],attribute= resInfo[x][2],value= resData[x]))
            if len(resData[x])>1:
                highData[resInfo[x][4]] = resData[x]
            else:
                highData[resInfo[x][4]] = resData[x][0]
            
    #conv = convert(highData,db)
    #print("High Data: ",highData)
    convMongo = convertMongo(highData)
    nfi = ietf_i2nsf_nsf_facing_interface()

    provisioning = coverSetNSF(convMongo)
    #print("Provisioning: ",provisioning)
    result = generate(nfi,provisioning)
    if isinstance(result,str):
        return {"ERROR":"NSF not Found"}
    return result

