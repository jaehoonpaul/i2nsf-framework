#!/bin/bash

# This will get the root node of data tree

CLIENT_CERT="/works/jetconf/data/example-client.pem"

echo "--- GET /restconf/data"
URL="https://localhost:8443/restconf/data/i2nsf:Policy/"
LD_LIBRARY_PATH=/usr/local/lib /usr/local/bin/curl --ipv4 --http2 -k --cert-type PEM -E "$CLIENT_CERT" -X GET "$URL"

