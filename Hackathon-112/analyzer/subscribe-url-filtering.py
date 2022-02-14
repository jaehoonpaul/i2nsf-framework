import os
from ncclient import manager
from xml.dom import minidom
from datetime import datetime, timezone

confd = {'address': '10.0.0.13',
           'netconf_port': 2022,
           'username': 'admin',
           'password': 'admin'}

confd_manager = manager.connect(
        host = confd["address"],
        port = confd["netconf_port"],
        username = confd["username"],
        password = confd["password"],
        hostkey_verify = False)

confd_manager.create_subscription(stream_name="I2NSF-Monitoring")

while True:
	print('Waiting for next notification')

	# This will block until a notification is received because
	# we didn't pass a timeout or block=False
	n = confd_manager.take_notification()
	local_time = datetime.now(timezone.utc).astimezone()
	print("Current Time: {} ".format(local_time.isoformat()))
	with open('log/moniter.xml','w+') as f:
	#with open('logs.xml','w+') as f:
		f.write(n.notification_xml)
	print(n.notification_xml)
  
#	os.system('python send.py')
#	print('SENDING FEEDBACK TO SECURITY CONTROLLER')

#os.system('netconf-console --host 10.0.0.4 -s all sub.xml')