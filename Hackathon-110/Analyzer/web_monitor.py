from flask import Flask, request, render_template, redirect, url_for, jsonify
import requests
import json

app = Flask(__name__)

@app.route("/")
def form():
	return render_template('my-form.html')

@app.route("/", methods=['POST'])
def my_form_post():
	text = request.form['text']
	return redirect(url_for('get_interface',iface=text))

@app.route("/monitor/<iface>", methods=['GET'])
def get_interface(iface):
	if iface=='url_filtering':
		r = requests.get('http://10.0.0.13:8888/monitor')
	elif iface=='time_based_firewall':
		r = requests.get('http://10.0.0.11:8888/monitor')
	else:
		return (redirect(url_for('not_found')))
	jMonitor=r.json()
	print (jMonitor)
	counter= jMonitor
	#print(json.dumps(counter, sort_keys=True, indent=4))
	print(jsonify(**counter))
	return (jsonify(**counter))

@app.route("/empty")
def not_found():
	return ("ERROR NOT FOUND")

if __name__ == '__main__':
	app.config['JSONIFY_PRETTYPRINT_REGULAR'] = True
	app.run(host='10.0.0.23')
