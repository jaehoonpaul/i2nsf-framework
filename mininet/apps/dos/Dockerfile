FROM ubuntu:trusty

RUN apt-get update && apt-get install -y \
    python3-pip \
    curl

RUN pip3 install scapy flask
COPY app.py dos.py /root/

ENV LC_ALL=C.UTF-8
ENV LANG=C.UTF-8
EXPOSE 5000

WORKDIR /root

CMD ["flask", "run", "--host='0.0.0.0'"]
