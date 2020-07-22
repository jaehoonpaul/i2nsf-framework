import os
import time

os.system("tacker vnfd-create --vnfd-file time_based_firewall_vnfd.yaml time_based_firewall_vnfd")
time.sleep(10)

os.system("tacker vnfd-create --vnfd-file url_filtering_vnfd.yaml url_filtering_vnfd")
time.sleep(10)

