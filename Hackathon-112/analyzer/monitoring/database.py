import mysql.connector
from datetime import date, datetime, timedelta

connection = mysql.connector.connect(
                host="10.0.0.17",
                port="3306",
                user="root",
                password="secu",
                database="monitoring",
            )

cursor = connection.cursor(prepared=True)

#date

#show all data table
#cursor.execute("SHOW TABLES")
#insert data example

current_Date = datetime.now()
format = current_Date.strftime('%Y-%m-%d %H:%M:%S')
print(format)

# test data input
insertData = ("""INSERT INTO `i2nsf-system-res-util-log` 
              (eventTime,`nsf-name`,`system-status`,`memory-usage`, `cpu-usage`, `disk-usage`,`disk-left`, `out-traffic-speed`,`in-traffic-speed`, `acquisition-method`, `emission-type`,`dampening-type`)
              VALUES %(evenTime)s, %(`nsf-name`)s, %(`system-status`)s,%(`memory-usage`)d,%(`cpu-usage`)s',%(`disk-usage`)s,%(`disk-lift`)s,%(`out-traffic-speed`)s,%(`in-traffic-speed`)s,%(`acquisition-method`)s, %(`emission-type`)s, %(`dampening`)s)""")

#cursor.execute(insertData)

tempdata = {
  'eventTime' : format,
  'nsf-name': "name",
  'system-status':"nomal",
  'memory-usage': 90,
  'cpu-usage' : 10,
  'disk-usage' : 9,
  'disk-left' : 3,
  'out-traffic-speed' :10,
  'in-traffic-speed' :90,
  'acquisition-method':"test",
  'emission-type':"test",
  'dampening-type':"test",
}

print(tempdata)

cursor.execute(insertData,tempdata);
connection.commit()


#print(cursor.recount,"record insert")
#print(datetime.datetime.today())

