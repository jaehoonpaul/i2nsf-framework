import os
import time


os.system("tacker vnffg-delete block_sns")
time.sleep(3)
os.system("tacker vnf-delete time_based_firewall_vnf")
time.sleep(3)
os.system("tacker vnf-delete url_filtering_vnf")
