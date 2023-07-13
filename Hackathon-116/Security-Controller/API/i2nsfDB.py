# -*- coding: utf-8 -*-
"""
Created on Wed May 18 15:51:58 2022

@author: Patrick
"""

import MySQLdb
import mapper

#nsf_facing = cfg.cfg_construction('DataModel/nfi.txt')


# convert = DataConverter(consumer[1],nsf_facing[1])
# convert.inputExtractedData(extract)

# convert.matchData(0, 0)   # match policy-name
# convert.matchData(1, 4)   # match rule-name
# convert.matchData(15, 118) # match packet-per-second
# convert.matchData(20, 120) # match ingress-action
# convert.matchData(20, 121) # match egress-action

# print(convert.requiredlist)

"""
DATABASE CREATE TABLE COMMANDS:
user-group     --> CREATE TABLE user (name VARCHAR(100) primary key, mac_address CHAR(12), start_ipv4_address VARCHAR(15), end_ipv4_address VARCHAR(15), start_ipv6_address VARCHAR(39), end_ipv6_address VARCHAR(39));
device-group   --> CREATE TABLE device (name VARCHAR(100) primary key, start_ipv4_address VARCHAR(15), end_ipv4_address VARCHAR(15), start_ipv6_address VARCHAR(39), end_ipv6_address VARCHAR(39), application_protocol VARCHAR(20));
location-group --> CREATE TABLE location (name VARCHAR(100) primary key, ipv4_prefix VARCHAR(18),ipv6_prefix VARCHAR(43), continent VARCHAR(20));
url-group      --> CREATE TABLE url (name VARCHAR(100), url VARCHAR(2048), PRIMARY KEY(name, url));
"""

# Initialize Database for endpoint group.
# The parameter 'db' is the detail of the database in dictionary to connect to MySQL DB
# example:
# db = {'host': 'localhost', 'user': 'user', 'passwd':'password'}
def initializeDB(db):
    mySQL = MySQLdb.connect(host   = db['host'],
                            user   = db['user'],
                            passwd = db['passwd'])
    cursor = mySQL.cursor()
    
    # Database for Endpoint Group (Consumer-Facing Interface Registration)
    cursor.execute("CREATE DATABASE endpoint;")
    cursor.execute("CREATE DATABASE mapping;")
    cursor.execute("CREATE TABLE endpoint.user (name VARCHAR(100) primary key, mac_address CHAR(12), start_ipv4_address VARCHAR(15), end_ipv4_address VARCHAR(15), start_ipv6_address VARCHAR(39), end_ipv6_address VARCHAR(39));")
    cursor.execute("CREATE TABLE endpoint.device (name VARCHAR(100) primary key, start_ipv4_address VARCHAR(15), end_ipv4_address VARCHAR(15), start_ipv6_address VARCHAR(39), end_ipv6_address VARCHAR(39), application_protocol VARCHAR(20));")
    cursor.execute("CREATE TABLE endpoint.location (name VARCHAR(100) primary key, ipv4_prefix VARCHAR(18),ipv6_prefix VARCHAR(43), continent VARCHAR(20));")
    cursor.execute("CREATE TABLE endpoint.url (name VARCHAR(100), url VARCHAR(2048), PRIMARY KEY(name, url));")
    
    # Database for Data Model Mapper (Attributes mapping between Consumer-Facing Interface and NSF-Facing Interface)
    cursor.execute("CREATE TABLE mapping.attributes (cfiID smallint(3), cfiPath VARCHAR(1000), nfiID smallint(3), nfiPath VARCHAR(1000));")
    
    mySQL.close()
    
# Insert Data for endpoint-group/user-group
def insertUserDB(db,data):
    mySQL = MySQLdb.connect(host   = db['host'],
                            user   = db['user'],
                            passwd = db['passwd'],
                            db     = 'endpoint')
    keys = list(data.keys())
    values = ["\'"+val+"\'" for val in data.values()]
    cursor = mySQL.cursor()
    try:
        cursor.execute("INSERT INTO user ({}) VALUES ({});".format(','.join(keys),','.join(values)))
        mySQL.commit()
        print("Successful Data Entry for user-group")
        mySQL.close()
        return 1
    except (MySQLdb.Error, MySQLdb.Warning) as err:
        print("MySQL Error: {}".format(err))
        mySQL.close()
        return 0
    
# Insert Data for endpoint-group/url-group
def insertURLDB(db,data):
    mySQL = MySQLdb.connect(host   = db['host'],
                            user   = db['user'],
                            passwd = db['passwd'],
                            db     = 'endpoint')
    keys = list(data.keys())
    cursor = mySQL.cursor()
    for url in data['url']:
        try:
            url = "\'"+url+"\'"
            cursor.execute("INSERT INTO url ({}) VALUES ({},{});".format(','.join(keys),"\'"+urlData['name']+"\'",url))
        except (MySQLdb.Error, MySQLdb.Warning) as err:
            print("MySQL Error: {}".format(err))
            mySQL.close()
    mySQL.commit()
    print("Successful Data Entry for url-group")
    mySQL.close()
    
def insertAttributesMap(db,cfiTree,nfiTree):
    mapResult = mapper.mapAttributes(cfiTree,nfiTree)
    mySQL = MySQLdb.connect(host   = db['host'],
                            user   = db['user'],
                            passwd = db['passwd'],
                            db     = 'mapping')
    cursor = mySQL.cursor()
    for keys,values in mapResult.items():
        keyPath = "\'"+keys.path()+"\'"
        for val in values:
            valPath = "\'"+val.path()+"\'"
            cursor.execute("INSERT INTO attributes (cfiID,cfiPath,nfiID,nfiPath) VALUES ({},{},{},{});".format(keys.id,keyPath,val.id,valPath))
    mySQL.commit()
    mySQL.close()

# Map the High-Level Attribute to the Low-Level Attribute
# Use the provided mapper in the Database
def mapAttr(highAttr,db):
    mySQL = MySQLdb.connect(host   = db['host'],
                            user   = db['user'],
                            passwd = db['passwd'],
                            db     = 'mapping')
    cursor = mySQL.cursor()
    cursor.execute("SELECT nfiID,nfiPath FROM attributes WHERE cfiID = {};".format(highAttr))
    res = cursor.fetchall()
    mySQL.close()
    return res

# Get Endpoint Data, i.e., user-endpoint, from the database.
def getUserData(highData,db):
    mySQL = MySQLdb.connect(host   = db['host'],
                            user   = db['user'],
                            passwd = db['passwd'],
                            db     = 'endpoint')
    cursor = mySQL.cursor()
    cursor.execute("SELECT * FROM user WHERE name = '{}';".format(highData))
    res = cursor.fetchone()
    mySQL.close()
    return res

def getURLData(highData,db):
    mySQL = MySQLdb.connect(host   = db['host'],
                            user   = db['user'],
                            passwd = db['passwd'],
                            db     = 'endpoint')
    cursor = mySQL.cursor()
    cursor.execute("SELECT * FROM url WHERE name = '{}';".format(highData))
    res = cursor.fetchall()
    mySQL.close()
    return res


#db = {'host': '127.0.0.1', 'user': 'patrick', 'passwd':'patrick'}
#initializeDB(db)
#insertAttributesMap(db,'DataModel/cfi_minus.txt','DataModel/nfi.txt')


#insertUserDB(db,data)

