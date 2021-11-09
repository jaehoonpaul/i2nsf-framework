#!/bin/sh
user=root
password=secu
mysql --user="$user" --password="$password" --execute="CREATE DATABASE nsfdb"

