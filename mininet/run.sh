#!/bin/bash
I2NSF_LIST=("mysqldb" "registration" "jetconf" "translator" "apache2" "dms")
for ITEM in ${I2NSF_LIST[@]}; do
  echo "docker-compose up ${ITEM}"
  docker-compose up -d ${ITEM}
  sleep 10
done
