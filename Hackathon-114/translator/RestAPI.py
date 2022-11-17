import pymongo
from regex import R
import mapper
from pprint import pprint
import json
from flask import Flask,request
from bson.json_util import dumps
from flask_cors import CORS
from dict2xml import dict2xml
import generator

companies = [{"id":1, "name": "Company One"}, {"id": 2, "name": "Company Two"}]

api = Flask(__name__)
CORS(api)

@api.route('/url/get', methods = ['GET'])
def restGetURLGroup():
    query = request.json
    client = pymongo.MongoClient("mongodb://115.145.178.185:27017/")
    db = client["endpoint"]
    col = db["url"]
    
    query = {query} #{"name":key}
    res = col.find_one(query)
    
    return json.loads(dumps(res))

@api.route('/url/put', methods = ['PUT'])
def restInsertURLGroup():
    try:
        data = request.json
        client = pymongo.MongoClient("mongodb://115.145.178.185:27017/")
        db = client["endpoint"]
        col = db["url"]
        
        res = col.insert_one(data)
        return "Success"
    except pymongo.errors.DuplicateKeyError:
        print("Duplicate Key for ",data["name"])

@api.route('/user/put', methods = ['PUT'])
def restInsertUserGroup():
    try:
        data = request.json
        client = pymongo.MongoClient("mongodb://115.145.178.185:27017/")
        db = client["endpoint"]
        col = db["user"]
        
        res = col.insert_one(data)
        return "Success"
    except pymongo.errors.DuplicateKeyError:
        return "Duplicate Key for ",data["name"]
        

@api.route('/user/get', methods = ['GET'])
def restGetUserGroup():
    
    client = pymongo.MongoClient("mongodb://115.145.178.185:27017/")
    db = client["endpoint"]
    col = db["user"]
    query = request.json
    res = col.find_one(query)
    return res

@api.route('/location/put', methods = ['PUT'])
def restInsertLocationGroup():
    try:
        data = request.json
        client = pymongo.MongoClient("mongodb://115.145.178.185:27017/")
        db = client["endpoint"]
        col = db["location"]
        
        res = col.insert_one(data)
        return "Success"
    except pymongo.errors.DuplicateKeyError:
        print("Duplicate Key for ",data["name"])

@api.route('/location/get', methods = ['GET'])
def restGetLocationGroup():
    client = pymongo.MongoClient("mongodb://115.145.178.185:27017/")
    db = client["endpoint"]
    col = db["location"]
    query = request.json
    res = col.find_one(query)
    return res

@api.route('/high_level', methods=['PUT'])
def restInsertConfiguration():
    req = request.json
    data = cleanNullTerms(req)
    print(data)
    xml = dict2xml(data)
    result = generator.gen(xml)
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