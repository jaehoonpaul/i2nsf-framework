xximport mysql.connector
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
insertData = ("INSERT INTO `i2nsf-system-res-util-log` "
              "(eventTime,`nsf-name`,`system-status`,`memory-usage`, `cpu-usage`, `disk-usage`,`disk-left`, `out-traffic-speed`,`in-traffic-speed`, `acquisition-method`, `emission-type`,`dampening-type`)"
              "VALUES (%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s)")

#cursor.execute(insertData)
tempdata = (format,'test','test','1','2','3','4','5','6','test','test','test')
print(tempdata)
cursor.execute(insertData,tempdata);
connection.commit()

#print(cursor.recount,"record insert")
#print(datetime.datetime.today())

