import os
import time

os.system("tacker vnf-create --vnfd-name time_based_firewall_vnfd time_based_firewall_vnf")
time.sleep(5)

os.system("tacker vnf-create --vnfd-name url_filtering_vnfd url_filtering_vnf")
time.sleep(5)

os.system("tacker vnffg-create --vnffgd-template vnffg_test.yaml block_sns")
