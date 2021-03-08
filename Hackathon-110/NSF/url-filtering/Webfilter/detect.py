import socket
import struct

from datetime import datetime

s = socket.socket(socket.PF_PACKET, socket.SOCK_RAW, 8)

mydict = {}

No_of_IPs = 15
R_No_of_IPs = No_of_IPs +10
while True:
	file_txt = open("attack_DDoS.txt",'a')
        pkt = s.recvfrom(2048)
        ipheader = pkt[0][14:34]
        ip_hdr = struct.unpack("!8sB3s4s4s",ipheader)
        IP = socket.inet_ntoa(ip_hdr[3])
        if IP != '172.24.4.1' and IP != '127.0.0.1' and IP != '10.0.0.14':
#               print "The Source of the IP is:", IP
                if mydict.has_key(IP): #IF THE IP HAS SENT A PACKET, START COUNTING THE NUMBER OF RECEIVED PACKETS
                        print mydict
                        mydict[IP]['count']+=1
                        currenttime = datetime.now()
                        if (mydict[IP]['count']/(currenttime - mydict[IP]['time']).total_seconds()) > 100: # IF THE RECEIVED PACKETS PER SECOND IS > 100 packets/second --> THE SENDER IS DOING A DDOS ATTACK
#                               print mydict[IP]['count']/(currenttime - mydict[IP]['time']).total_seconds()
                                if mydict[IP]['ddos']:
                                        mydict[IP]['ddos_time_start'] = currenttime
                                mydict[IP]['ddos_time_end'] = currenttime
                                line = "DDOS attack is Detected: "
                                t1 = str(datetime.now())
                                print line,IP
                                if not mydict[IP]['ddos']:
                                        file_txt.writelines(t1)
                                        file_txt.writelines(" ")
                                        file_txt.writelines(IP)
                                        file_txt.writelines("\n")
                                        mydict[IP]['ddos'] = True
                        elif (currenttime - mydict[IP]['time']).total_seconds() >= 5:
                                mydict[IP]['count'] = 0
                                mydict[IP]['time'] = currenttime
                                mydict[IP]['ddos'] = False
                else:
                        mydict[IP] = {'count': 1,
                                        'time': datetime.now(),
                                        'ddos': False,
                                        'ddos_time_start': None}

	file_txt.close()
