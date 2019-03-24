#!/usr/bin/env bash


cp templates/ntp.conf.in /etc/ntp.conf
mkdir -p /etc/ntp
touch /etc/ntp/ntp-servers.conf

#cd /etc


#rm -f ntpkey*linuxcfg*
#ntp-keygen -M -s linuxcfg
#cp ntpkey_MD5key_linuxcfg.* ntp.keys
#echo "1 MD5 secret" > /etc/ntp.keys

echo "Trying to restart ntp. "
echo "If this doesn't work it must be done manually."
service ntp restart
