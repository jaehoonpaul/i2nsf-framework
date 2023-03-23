import pymongo
from regex import R
from pprint import pprint
import json
from flask import Flask,request
from bson.json_util import dumps
from flask_cors import CORS
from dict2xml import dict2xml
import generatorv2
from flask import Response


companies = [{"id":1, "name": "Company One"}, {"id": 2, "name": "Company Two"}]

api = Flask(__name__)
CORS(api)

@api.route('/url/get', methods = ['GET'])
def restGetURLGroup():
    query = request.json
    client = pymongo.MongoClient("mongodb://172.17.20.250:27017/")
    db = client["endpoint"]
    col = db["url"]
    
    query = {query} #{"name":key}
    res = col.find_one(query)
    
    return json.loads(dumps(res))

@api.route('/url/put', methods = ['PUT'])
def restInsertURLGroup():
    try:
        data = request.json
        print(data)
        client = pymongo.MongoClient("mongodb://172.17.20.250:27017/")
        db = client["endpoint"]
        col = db["url"]
        
        res = col.insert_one(data)
        return "Success"
    except pymongo.errors.DuplicateKeyError:
        print("Duplicate Key for ",data["name"])

@api.route('/nsfDB/get', methods = ['GET'])
def restGetAllCapability(query={}):
    client = pymongo.MongoClient("mongodb://172.17.20.250:27017/")
    db = client["nsfDB"]
    col = db["capabilities"]
    result = {}
    result["nsf"] = []
    for res in col.find(query):
        result["nsf"].append(res)
    return json.loads(dumps(result))

@api.route('/user/put', methods = ['PUT'])
def restInsertUserGroup():
    try:
        data = request.json
        client = pymongo.MongoClient("mongodb://172.17.20.250:27017/")
        db = client["endpoint"]
        col = db["user"]
        
        res = col.insert_one(data)
        return "Success"
    except pymongo.errors.DuplicateKeyError:
        return "Duplicate Key for ",data["name"]
        

@api.route('/user/get', methods = ['GET'])
def restGetUserGroup():
    
    client = pymongo.MongoClient("mongodb://172.17.20.250:27017/")
    db = client["endpoint"]
    col = db["user"]
    query = request.json
    res = col.find_one(query)
    return res

@api.route('/location/put', methods = ['PUT'])
def restInsertLocationGroup():
    try:
        data = request.json
        client = pymongo.MongoClient("mongodb://172.17.20.250:27017/")
        db = client["endpoint"]
        col = db["location"]
        
        res = col.insert_one(data)
        return "Success"
    except pymongo.errors.DuplicateKeyError:
        print("Duplicate Key for ",data["name"])

@api.route('/location/get', methods = ['GET'])
def restGetLocationGroup():
    client = pymongo.MongoClient("mongodb://172.17.20.250:27017/")
    db = client["endpoint"]
    col = db["location"]
    query = request.json
    res = col.find_one(query)
    return res


# Insert Capabilities of an NSF. The DMS delivers the capabilities via Registration Interface
@api.route('/register/nsf', methods = ['PUT'])
def restInsertCapability():
    try:
        data = request.json
        client = pymongo.MongoClient("mongodb://172.17.20.250:27017/")
        db = client["nsfDB"]
        col = db["capabilities"]
        print(data)
        res = col.insert_one(data)
        return "Success"
    except pymongo.errors.DuplicateKeyError:
        print("Duplicate Key for ",data["nsf-name"])
        return Response(f"Duplicate Key for {data['nsf-name']}", status=400)


#API for security policy tranlator - Input High-level policy (CFI), Output Low-level policy (NFI)
#http://ipv4:5000/high_level
@api.route('/high_level', methods=['PUT'])
def restInsertConfiguration():
    req = request.json
    #start = datetime.datetime.now()
    data = cleanNullTerms(req)
    print(data)
    xml = dict2xml(data)
    result = generatorv2.gen(xml)
    #end = datetime.datetime.now()
    # time = end-start
    # result["time"] = time.total_seconds()
    # result["optimal"] = optimal.total_seconds()
    # for x,y in result.items():
    #     print(x)
    #     print(y)
    return result

def cleanNullTerms(d):
   clean = {}
   for k, v in d.items():
      if isinstance(v, dict):
         nested = cleanNullTerms(v)
         if len(nested.keys()) > 0:
            clean[k] = nested
      elif v is not None:
         clean[k] = v
   return clean

if __name__== '__main__':
    api.run(host="115.145.178.185")