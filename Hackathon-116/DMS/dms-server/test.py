# from ncclient import manager
# host = "192.168.56.104"

# with manager.connect(host=host, port=2022, username="admin", password="admin", hostkey_verify=False) as m:
#     c = m.get_config(source='running').data_xml
#     with open("%s.xml" % host, 'w') as f:
#         f.write(c)
