from flask import Flask
import os
import socket
import json

app = Flask(__name__)

s=socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
s.connect(('115.145.178.170',0))

@app.route("/interface")
def getInterface():
        os.system("vnstat -u -i ens3")
        status = os.popen("vnstat --json").read()
        jstatus = json.loads(status)
        received=json.dumps(jstatus['interfaces'][0]['traffic']['total']['rx'])
        transmit=json.dumps(jstatus['interfaces'][0]['traffic']['total']['tx'])
        counters = '[ { "interface-name": "ens3", "nsf-name": "time_based_firewall", "in_total_traffic_bytes":'+ received +', "out_total_traffic_bytes":'+ transmit +' } ] '
        return (counters)

if __name__ == '__main__':
        app.run(host=s.getsockname()[0],port=8888)