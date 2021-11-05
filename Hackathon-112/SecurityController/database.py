
import mysql.connector
connection = mysql.connector.connect(
                host="10.0.0.17",
                port="3306",
                user="root",
                password="secu",
                database="monitoring",
            )
cursor = connection.cursor()



