import MySQLdb

mydb = MySQLdb.connect(host="localhost", user="root")
mycur = mydb.cursor()

mycur.execute("DROP DATABASE nsfdb")
mycur.execute("CREATE DATABASE nsfdb")

mycur.execute("USE nsfdb")

mycur.execute("CREATE TABLE nsftable (nname VARCHAR(255), processing VARCHAR(30), outbound VARCHAR(30), inbound VARCHAR(30), initiated VARCHAR(5))")
mycur.execute("CREATE TABLE capabilitytable (nname VARCHAR(255), cname VARCHAR(255))")
mycur.execute("CREATE TABLE fieldtable (cname VARCHAR(255), fieldname VARCHAR(255))")
mycur.execute("CREATE TABLE endpointtable (ename VARCHAR(255), id INT(1), data VARCHAR(255))")

mydb.commit()
mycur.close()
mydb.close()

print("MySQL Initialization")
