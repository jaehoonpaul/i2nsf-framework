FROM ubuntu:18.04

RUN apt-get -y update && apt-get -y upgrade
RUN apt-get -y install python3-pip
RUN python3 -m pip install jetconf

ADD ./jetconf.tar /home/ubuntu/works/

WORKDIR /home/ubuntu/works/jetconf

EXPOSE 8443

CMD python3 run.py -c example-config.yaml

