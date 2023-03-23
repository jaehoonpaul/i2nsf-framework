# I2NSF Hackathon Manual
### March 25-26, 2023

**Made by: Patrick Lingga (SKKU)**

**Champion: Jaehoon Paul Jeong (SKKU)**

## Environment

* OS: Ubuntu 20.04 LTS (64-bit)
* Openstack: Queens version
* ConfD: version 8.0.2 
* MySQL: version 8.0.32 
* MongoDB: version 3.6.8
* Where to get code:
https://github.com/jaehoonpaul/i2nsf-framework

# Security Controller
Note: change the IP address to actual address of the server

1. RUN Security Controller Translator API:

```
$ cd Security-Controller/API
$ python3 insertMongoDB.py #INSERT INITIAL DATA MODEL MAPPER
$ python3 RestAPI.py
```

2. Run Security Controller GUI:

```
$ cd Security-Controller/react
$ npm start
```

# DMS

1. Run the DMS server:

```
$ cd DMS/dms-server
$ make clean all start
```