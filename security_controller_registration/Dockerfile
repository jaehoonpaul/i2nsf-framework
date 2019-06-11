FROM ubuntu:18.04

RUN apt-get -y update
RUN apt-get -y install make
RUN apt-get -y install gcc
RUN apt-get -y install ssh

ADD ./confd-basic-6.6.linux.x86_64.tar.gz /home/ubuntu/
ADD ./Registration /home/ubuntu/registration

RUN /home/ubuntu/confd-basic-6.6.linux.x86_64/confd-basic-6.6.linux.x86_64.installer.bin /home/ubuntu/confd-6.6/


WORKDIR /home/ubuntu/registration

EXPOSE 2022

CMD make clean all start
