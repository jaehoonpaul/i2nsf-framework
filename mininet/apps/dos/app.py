from flask import Flask, request
import subprocess

app = Flask(__name__)

@app.route('/')
def index():
   return 'Hello'

@app.route('/dos')
def dos():
   dest = request.args.get('dest')
   pid = subprocess.Popen(['python3', 'dos.py', dest], stdout=subprocess.PIPE, stderr=subprocess.PIPE, stdin=subprocess.PIPE)
   return 'Done'
