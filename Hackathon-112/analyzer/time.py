# -*- coding: utf-8 -*-
import mysql.connector
import pprint
import xmltodict
import json
from datetime_z import parse_datetime
import threading
import os
from ncclient import manager
from xml.dom import minidom
from datetime import datetime, timezone

confd = {'address': '10.0.0.30',
         'netconf_port': 2022,
         'username': 'admin',
         'password': 'admin'}

confd_manager = manager.connect(
    host=confd["address"],
    port=confd["netconf_port"],
    username=confd["username"],
    password=confd["password"],
    hostkey_verify=False)

confd_manager.create_subscription(stream_name="I2NSF-Monitoring")

while True:
    print('Waiting for next notification')

    # This will block until a notification is received because
    # we didn't pass a timeout or block=False
    n = confd_manager.take_notification()
    local_time = datetime.now(timezone.utc).astimezone()
    print("Current Time: {} ".format(local_time.isoformat()))
    with open('log/monitor.xml', 'w+') as f:
    #with open('logs.xml','w+') as f:
        f.write(n.notification_xml)

    # test data input
    insertData = ("INSERT INTO `i2nsf-system-res-util-log` "
                "(eventTime,`nsf-name`,`system-status`,`memory-usage`, `cpu-usage`, `disk-usage`,`disk-left`, `out-traffic-speed`, `in-traffic-speed`, `acquisition-method`, `emission-type`,`dampening-type`)"
                "VALUES (%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s)")

    mydict = xmltodict.parse(n.notification_xml)
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

    print('Tread running - ')
    tempdata = (eventTime, nsf_name, system_status, memory_usage, cpu_usage, disk_usage, disk_left,
                out_traffic_speed, in_traffic_speed, acquisition_method, emission_type, dampening_type)
    connection = mysql.connector.connect(
        host="10.0.0.17",
        port="3306",
        user="root",
        password="secu",
        database="monitoring",
    )
    cursor = connection.cursor(prepared=True)
    cursor.execute(insertData, tempdata)
    connection.commit()
    connection.close()
    print('-----Complete Insert monitoring XML Data------')
    print(n.notification_xml)
    





