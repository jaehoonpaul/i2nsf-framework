#!/bin/sh

sudo apt-get update
sudo add-apt-repository ppa:deadsnakes/ppa
sudo apt-get update
sudo apt-get install -y  python python-pip python-mysqldb python-dev libmysqlclient-dev mysql-client-core-5.7 libxml2-utils mysql-server apache2 php-pear libapache2-mod-php php-mysql php-fpm php-cli php-mysqlnd php-pgsql php-sqlite3 php$

export LC_ALL="en_US.UTF-8"
export LC_ALL="en_US.UTF-8"
pip install numpy==1.14.6 MySQL-python paramiko
cp -r /home/ubuntu/i2nsf-framework/Hackathon-108/SecurityController/* /home/ubuntu/
cd /home/ubuntu/confd-basic-6.6.linux.x86_64/
sh confd-basic-6.6.linux.x86_64.installer.bin /home/ubuntu/confd-6.6
source /home/ubuntu/confd-6.6/confdrc

cd /home/ubuntu
tar -xvf jetconf.tar
mkdir works
mv jetconf /home/ubuntu/works/jetconf

cd /home/ubuntu/works/jetconf
pip install -r requirements.txt
python3 -m pip install .

cd /home/ubuntu

sudo cp -r /home/ubuntu/security_controller_web-v2/html /var/www/
sudo chown ubuntu:ubuntu /var/www/html
sudo chown ubuntu:ubuntu /var/www/html/*
sudo chmod +x /home/ubuntu/clean_security_controller
sudo chmod 777 /home/ubuntu/XML
sudo chmod 777 /home/ubuntu/HighLevelPolicy
sudo chmod 777 /var/www/html

user=root
password=secu
mysql --user="$user" --password="$password" --execute="CREATE DATABASE nsfdb"

python initializeDB.py
