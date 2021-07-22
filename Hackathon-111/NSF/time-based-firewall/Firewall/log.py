import json
from datetime import datetime
import os
import psutil

os.system("sudo vnstat -u -i ens3")
interface = os.popen("vnstat --json").read()
jsonIf = json.loads(interface)
#print(json.dumps(jsonIf, indent=4, sort_keys=True))
#Interface
received=json.dumps(jsonIf['interfaces'][0]['traffic']['total']['rx'])
transmit=json.dumps(jsonIf['interfaces'][0]['traffic']['total']['tx'])

#Disk
hdd = psutil.disk_usage('/')

#CPU
cpu = psutil.cpu_percent()

#Memory
mem = psutil.virtual_memory().percent

log = {
	"system_status": 'Running',
	"cpu_usage": cpu,
	"memory_usage": mem,
	"disk_usage": hdd.percent,
	"disk_left": round(hdd.free*100/float(hdd.total),1),
	"in_traffic_speed": int(received),
	"out_traffic_speed": int(transmit)
}
#print(json.dumps(log, indent=4, sort_keys=True))


date = datetime.today().strftime('%Y-%m-%d')
with open('resource/'+date+'log.json','w+') as fi:
	fi.write(json.dumps(log, indent=4, sort_keys=True))
