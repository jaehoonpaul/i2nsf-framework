from json2xml import json2xml
from json2xml.utils import readfromurl, readfromstring, readfromjson

# get the data from a file
data = readfromjson("/home/ubuntu/regi_ip.json")
print(json2xml.Json2xml(data,wrapper="all",pretty=True,attr_type=False).to_xml())
