# -*- coding: utf-8 -*-
import mysql.connector
import pprint
import xmltodict
import json
from datetime_z import parse_datetime
import threading


mydict2= xmltodict.parse("""<?xml version="1.0" encoding="UTF-8"?>
<notification xmlns="urn:ietf:params:xml:ns:netconf:notification:1.0">
<eventTime>2021-07-20T02:33:15.035438+00:00</eventTime>
<i2nsf-log xmlns='urn:ietf:params:xml:ns:yang:ietf-i2nsf-nsf-monitoring'>
  <i2nsf-system-res-util-log>
    <system-status>Running</system-status>
    <cpu-usage>66</cpu-usage>
    <memory-usage>17</memory-usage>
    <disk-usage>63</disk-usage>
    <disk-left>36</disk-left>
    <in-traffic-speed>50635</in-traffic-speed>
    <out-traffic-speed>7225</out-traffic-speed>
    <acquisition-method xmlns:nsfmi="urn:ietf:params:xml:ns:yang:ietf-i2nsf-nsf-monitoring">nsfmi:subscription</acquisition-method>
    <emission-type xmlns:nsfmi="urn:ietf:params:xml:ns:yang:ietf-i2nsf-nsf-monitoring">nsfmi:periodical</emission-type>
    <dampening-type xmlns:nsfmi="urn:ietf:params:xml:ns:yang:ietf-i2nsf-nsf-monitoring">nsfmi:on-repetition</dampening-type>
    <nsf-name>url_filtering</nsf-name>
  </i2nsf-system-res-util-log>
</i2nsf-log>
</notification>
""")
#read xml file
f =  open("/home/ubuntu/log/moniter.xml",'r',encoding='utf-8') 
content = f.read()
print(content)


mydict = xmltodict.parse(content)
#print(content)
times = mydict['notification']['eventTime']

eventTime = parse_datetime(times)
print(eventTime)
 
system_status = mydict['notification']['i2nsf-log']['i2nsf-system-res-util-log']['system-status']

cpu_usage = mydict['notification']['i2nsf-log']['i2nsf-system-res-util-log']['cpu-usage']

memory_usage = mydict['notification']['i2nsf-log']['i2nsf-system-res-util-log']['memory-usage']

disk_usage = mydict['notification']['i2nsf-log']['i2nsf-system-res-util-log']['disk-usage']

disk_left = mydict['notification']['i2nsf-log']['i2nsf-system-res-util-log']['disk-left']

in_traffic_speed = mydict['notification']['i2nsf-log']['i2nsf-system-res-util-log']['in-traffic-speed']

out_traffic_speed = mydict['notification']['i2nsf-log']['i2nsf-system-res-util-log']['out-traffic-speed']

acquisition_method = mydict['notification']['i2nsf-log']['i2nsf-system-res-util-log']['acquisition-method']['#text']

emission_type = mydict['notification']['i2nsf-log']['i2nsf-system-res-util-log']['emission-type']['#text']

dampening_type = mydict['notification']['i2nsf-log']['i2nsf-system-res-util-log']['dampening-type']['#text']

nsf_name = mydict['notification']['i2nsf-log']['i2nsf-system-res-util-log']['nsf-name']
print('eventTime         : {}'.format(eventTime))
print('system-status     : {}'.format(system_status))
print('cpu-usage         : {}'.format(cpu_usage))
print('memory-usage      : {}'.format(memory_usage))
print('disk-usage        : {}'.format(disk_usage))
print('disk-left         : {}'.format(disk_left))
print('in-traffic-speed  : {}'.format(in_traffic_speed))
print('out-traffic-speed : {}'.format(out_traffic_speed))
print('acquisition-method: {}'.format(acquisition_method))
print('emission-type     : {}'.format(emission_type))
print('dampening-type    : {}'.format(dampening_type))
print('nsf-name          : {}'.format(nsf_name))



connection = mysql.connector.connect(
                host="10.0.0.17",
                port="3306",
                user="root",
                password="secu",
                database="monitoring",
            )

cursor = connection.cursor(prepared=True)

# test data input
insertData = ("INSERT INTO `i2nsf-system-res-util-log` "
              "(eventTime,`nsf-name`,`system-status`,`memory-usage`, `cpu-usage`, `disk-usage`,`disk-left`, `out-traffic-speed`,`in-traffic-speed`, `acquisition-method`, `emission-type`,`dampening-type`)"
              "VALUES (%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s)")

#cursor.execute(insertData)
def thread_run():
    print('Tread running - ')
		tempdata = (eventTime,nsf_name,system_status,memory_usage,cpu_usage,disk_usage,disk_left,out_traffic_speed,in_traffi$
		cursor.execute(insertData,tempdata);
		connection.commit()
    threading.Timer(2.5, thread_run).start()
		connection.commit()
thread_run()

#tempdata = (eventTime,nsf_name,system_status,memory_usage,cpu_usage,disk_usage,disk_left,out_traffic_speed,in_traffic_speed,acquisition_method,emission_type,dampening_type)
#print(tempdata)
#cursor.execute(insertData,tempdata);

#connection.commit()

#print(cursor.recount,"record insert")
#print(datetime.datetime.today())
