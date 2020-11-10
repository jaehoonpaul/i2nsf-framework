#!/bin/sh
sudo apt-get update
sudo apt-get install -y software-properties-common
sudo add-apt-repository ppa:oisf/suricata-stable
sudo apt-get update
sudo apt-get install -y gcc make python-minimal python3 zip python-pip
sudo chmod +x test_confd init_confd init_suricata
export LC_ALL="en_US.UTF-8"
export LC_CTYPE="en_US.UTF-8"

pip install paramiko
unzip Firewall.zip
unzip confd-basic-6.6.linux.x86_64.zip

./confd-basic-6.6.linux.x86_64/confd-basic-6.6.linux.x86_64.installer.bin /home/ubuntu/confd-6.6
sudo apt-get install -y suricata

echo '#!'"/bin/sh -e" > /etc/rc.local
echo "#
# rc.local
#
# This script is executed at the end of each multiuser runlevel.
# Make sure that the script will "exit 0" on success or any other
# value on error.
#
# In order to enable or disable this script just change the execution
# bits.
#
# By default this script does nothing.
/home/ubuntu/init_suricata
/home/ubuntu/init_confd
/home/ubuntu/test_confd
exit 0" >> /etc/rc.local
