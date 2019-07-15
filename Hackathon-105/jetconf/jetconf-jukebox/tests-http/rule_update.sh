#!/bin/bash

CLIENT_CERT="/works/jetconf/data/example-client.pem"

echo "--- DELETE album Wasting Time"
URL="https://127.0.0.1:8443/restconf/data/i2nsf:policy/I2NSF/Policy=1"
curl --http2 -k --cert-type PEM -E $CLIENT_CERT -X DELETE -d "$POST_DATA" "$URL"

POST_DATA="@example_policy2.json"
URL="https://127.0.0.1:8443/restconf/data/i2nsf:policy/I2NSF"
curl --http2 -k --cert-type PEM -E $CLIENT_CERT -X POST -d "$POST_DATA" "$URL" 

echo "--- conf-commit"
URL="https://127.0.0.1:8443/restconf/operations/jetconf:conf-commit"
curl --http2 -k --cert-type PEM -E $CLIENT_CERT -X POST "$URL"

