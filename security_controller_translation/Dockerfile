FROM ubuntu:18.04

RUN apt-get -y update
RUN apt-get -y install make
RUN apt-get -y install gcc
RUN apt-get -y install ssh
RUN apt-get -y install python3-pip
RUN apt-get -y install python-pip
RUN pip install --user numpy
RUN apt-get install -y python-dev libmysqlclient-dev
RUN pip install --user MySQL-python
RUN apt-get install -y mysql-server
RUN pip install paramiko
RUN apt-get install libxml2-utils
RUN apt-get -y install curl

ADD ./jetconf.tar /home/ubuntu/works/
ADD ./confd-basic-6.6.linux.x86_64.tar.gz /home/ubuntu/
RUN /home/ubuntu/confd-basic-6.6.linux.x86_64/confd-basic-6.6.linux.x86_64.installer.bin /home/ubuntu/confd-6.6/


ADD ./translation /home/ubuntu/translation
ADD ./i2nsf /home/ubuntu/translation/

WORKDIR /home/ubuntu/translation

EXPOSE 3306
EXPOSE 55552
EXPOSE 55560
EXPOSE 12345

CMD ./i2nsf
#CMD service mysql restart && python initializeDB.py && python server.py

#RUN service mysql restart
#RUN service mysql status
#RUN python initializeDB.py

#CMD python server.py
