import MySQLdb

nsfdb = MySQLdb.connect(host="localhost", user="root", passwd="secu", db="nsfdb")
nsfcur = nsfdb.cursor()

nsfcur.execute("CREATE TABLE nsftable (nname VARCHAR(255), processing VARCHAR(30), outbound VARCHAR(30), inbound VARCHAR(30), initiated VARCHAR(5))")
nsfcur.execute("CREATE TABLE capabilitytable (nname VARCHAR(255), cname VARCHAR(255))")
nsfcur.execute("CREATE TABLE fieldtable (cname VARCHAR(255), fieldname VARCHAR(255))")
nsfcur.execute("CREATE TABLE endpointtable (ename VARCHAR(255), id INT(1), data VARCHAR(255))")

nsfdb.commit()
nsfcur.close()
nsfdb.close()
