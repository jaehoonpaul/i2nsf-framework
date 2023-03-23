I2NSF SPT

To run the translation server, make sure MongoDB is installed.
The code was tested using MongoDB Version 3.6.8
To install MongoDB follow: https://www.mongodb.com/docs/manual/installation/

Installation Steps:
1. Change the IP address & port in i2nsfMongoDB.py (i.e., "mongodb://115.145.178.185:27017/") according to the IP address that provides MongoDB

2. Run MongoDB and insert the data to the database in insertMongoDB.py:
```sh
cd translator
python3 insertMongoDB.py
```

3. Make the unique indexes for the collections in MongoDB:
```
# Connect to the MongoDB
~ $ mongo
> use endpoint
> db.user.createIndex({"name":1},{unique:true})
> db.url.createIndex({"name":1},{unique:true})
```

4. Run the REST API for the Translator
```sh
cd translator
python3 RestAPI.py
```

5. Run the Webserver in the "react" directory (Read the README file)
