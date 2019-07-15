#!/bin/bash

CLIENT_CERT="/works/jetconf/data/example-client.pem"

echo "--- conf-start 1"
POST_DATA='{ "jetconf:input": {"name": "Edit 1", "options": "config"} }'
URL="https://127.0.0.1:8443/restconf/operations/jetconf:conf-start"
curl --http2 -k --cert-type PEM -E $CLIENT_CERT -X POST -d "$POST_DATA" "$URL"

echo "--- POST new policy"
POST_DATA="@test.json"
URL="https://127.0.0.1:8443/restconf/data/i2nsf:Policy/"
#curl --ipv4 --http2 -X POST -d "$POST_DATA" "$URL"
curl --http2 -k --cert-type PEM -E $CLIENT_CERT -X POST -d "$POST_DATA" "$URL"

echo "--- conf-commit"
URL="https://127.0.0.1:8443/restconf/operations/jetconf:conf-commit"
curl --http2 -k --cert-type PEM -E $CLIENT_CERT -X POST "$URL"

