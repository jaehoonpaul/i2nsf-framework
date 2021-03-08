<?php
$CLIENT_CERT = "/works/jetconf/data/example-client.pem";
$POST_DATA = "@test.json";
$URL = "https://localhost:8443/restconf/data/i2nsf:policy/I2NSF";
shell_exec("curl --ipv4 --http2 -k --cert-type PEM -E $CLIENT_CERT -X POST -d $POST_DATA $URL");
?>
