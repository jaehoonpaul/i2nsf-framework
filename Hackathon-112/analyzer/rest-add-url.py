# -*- coding: utf-8 -*-
import requests


jsonData = {
	'mNumber': "monitoring4",
	'cpu_usage': "85",
	'disk_left': "39",
	'disk_usage': "60",
	'eventTime': "2021-07-20T02:18:38.800515+00:00",
	'in_traffic': "1771999",
	'memory_usage': "82",
	'out_traffic': "464999",
	'system_status': "Running",
	'nsf_name': "url_filtering"
}

r = requests.post("http://172.24.4.14:5000/api/addcar/", json = jsonData);
