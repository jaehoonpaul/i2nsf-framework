#!/bin/bash
cd /home/deploy/Registration
pgrep -f "python3 run.py"|awk '{print $1}'|xargs sudo kill -9
pgrep -f "make clean all start"|awk '{print $1}'|xargs sudo kill -9
sudo make clean
pgrep -f "server.py"|awk '{print $1}'|xargs sudo kill -9
sleep 1s;
cd /home/deploy/jetconf
sudo python3 run.py -c example-config.yaml > /tmp/jetconf.log 2>&1 &
sleep 1s;
cd /home/deploy/revSC
sudo make clean
sleep 1s;
sudo python server.py > /tmp/SecurityController.log 2>&1 &
sleep 1s;
cd /home/deploy/Registration
sudo make clean all start > /tmp/confd.log 2>&1 &