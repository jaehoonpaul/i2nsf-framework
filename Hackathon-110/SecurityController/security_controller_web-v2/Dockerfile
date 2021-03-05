FROM ubuntu:18.04

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get -y update && apt-get -y upgrade && \ 
apt-get install -y php libapache2-mod-php  \
php-fpm php-cli php-mysqlnd php-pgsql php-sqlite3 php-redis \
php-apcu php-intl php-imagick php-json php-gd php-curl

RUN apt-get -y install curl


#RUN apt-get -y install python3-pip
#RUN python3 -m pip install jetconf

ADD ./jetconf.tar /home/ubuntu/works/
ADD ./html /var/www/html
ADD ./apache2.conf /etc/apache2/apache2.conf

WORKDIR /var/www/html

EXPOSE 80

CMD service apache2 start && tail -f /var/log/apache2/access.log
