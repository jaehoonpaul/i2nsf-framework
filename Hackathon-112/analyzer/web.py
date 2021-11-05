from flask import Flask, request, render_template, redirect, url_for, jsonify
import requests
import json
import os
import xmltodict

app = Flask(__name__)

@app.route("/")
def form():
	logs = sorted(os.listdir("/home/ubuntu/log"),reverse=True)
	eventTime = []
	alarmCategory = []
	acquisitionMethod = []
	emissionType = []
	usage = []
	threshold = []
	nsfName = []
	severity = []
	for log in logs:
		s = open("/home/ubuntu/log/"+log,"rb")
		data = xmltodict.parse(s)
		eventTime.append(data["notification"]["eventTime"])
		alarmCategory.append(data["notification"]["i2nsf-system-detection-alarm"]["alarm-category"]["#text"])
		acquisitionMethod.append(data["notification"]["i2nsf-system-detection-alarm"]["acquisition-method"]["#text"])
		emissionType.append(data["notification"]["i2nsf-system-detection-alarm"]["emission-type"]["#text"])
		usage.append(data["notification"]["i2nsf-system-detection-alarm"]["usage"])
		threshold.append(data["notification"]["i2nsf-system-detection-alarm"]["threshold"])
		nsfName.append(data["notification"]["i2nsf-system-detection-alarm"]["nsf-name"])
		severity.append(data["notification"]["i2nsf-system-detection-alarm"]["severity"])
	return render_template('notifDetail.html',
				eventTime=eventTime,
 				alarmCategory=alarmCategory,
				acquisitionMethod=acquisitionMethod,
				emissionType=emissionType,
				usage=usage,
				threshold=threshold,
				nsfName=nsfName,
				severity=severity
			)
#	return render_template('notification.html',notification_list=log)

@app.route("/<log>")
def getData(log):
	s = open("/home/ubuntu/log/"+log,"rb")
	data = xmltodict.parse(s)
	return render_template('notifDetail.html',
		eventTime=data["notification"]["eventTime"],
		alarmCategory=data["notification"]["i2nsf-system-detection-alarm"]["alarm-category"]["#text"],
		acquisitionMethod=data["notification"]["i2nsf-system-detection-alarm"]["acquisition-method"]["#text"],
		emissionType=data["notification"]["i2nsf-system-detection-alarm"]["emission-type"]["#text"],
		usage=data["notification"]["i2nsf-system-detection-alarm"]["usage"],
		threshold=data["notification"]["i2nsf-system-detection-alarm"]["threshold"],
		nsfName=data["notification"]["i2nsf-system-detection-alarm"]["nsf-name"],
		severity=data["notification"]["i2nsf-system-detection-alarm"]["severity"]
		)

#@app.route("/", methods=['POST'])
#def my_form_post():
#	text = request.form['text']
#	return redirect(url_for('get_interface',iface=text))

@app.route("/empty")
def not_found():
	return ("ERROR NOT FOUND")

if __name__ == '__main__':
	app.run(host='10.0.0.14',port=8000)
