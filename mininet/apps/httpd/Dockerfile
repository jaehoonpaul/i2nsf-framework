FROM httpd:2.4.46
RUN apt-get update && apt-get install -y \
    net-tools \
    iputils-ping \
    iproute2 \
    tcpdump \
    bash \
    iperf \
    iptables-persistent netfilter-persistent curl

COPY ./public-html/ /usr/local/apache2/htdocs/
COPY ./run.sh /usr/local/apache2/
WORKDIR /usr/local/apache2

CMD ["sh","run.sh"]
