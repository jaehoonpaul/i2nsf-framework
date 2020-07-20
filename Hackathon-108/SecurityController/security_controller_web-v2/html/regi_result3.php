<?php
function array_to_xml( $data, &$xml_data ) {
    foreach( $data as $key => $value ) {
        if( is_array($value) ) {
            if( is_numeric($key) ){
                $key = 'Policy_enterprise';
            }
            $subnode = $xml_data->addChild($key);
            array_to_xml($value, $subnode);
        } else {
            $xml_data->addChild($key, htmlspecialchars($value));
        }
    }
}

$url_name = $_POST['urlName'];
$url = $_POST['url'];
#$url_2 = $_POST['url_2'];


$payInfo = array("name" => $url_name, "content" => [$url_1, $url_2]);
$threatGroup["threat-prevention"] = array("payload-content" => [$payInfo]);
$groupData["ietf-i2nsf-cfi-policy:policy"] = $threatGroup;
file_put_contents("regi_url.json", json_encode($groupData, JSON_PRETTY_PRINT));

$CLIENT_CERT = "/home/ubuntu/works/jetconf/data/example-client.pem";
$POST_DATA = "@regi_url.json";
$URL = "https://localhost:8443/restconf/data/ietf-i2nsf-cfi-policy:i2nsf-cfi";
$log = shell_exec("/usr/local/bin/curl --ipv4 --http2 -k --cert-type PEM -E $CLIENT_CERT -X POST -d $POST_DATA $URL");

$dbname = "nsfdb";

$conn = mysqli_connect("127.0.0.1","root","secu",$dbname);

if (!$conn) {
  die("Connection failed: " . mysqli_connect_error());
}

$sql = "INSERT INTO endpointtable (ename, id, data) VALUES ('$url_name', 114, '$url')";

if (mysqli_query($conn, $sql)) {
  $logs="New record created successfully";
} else {
  $logs="ERROR";
}
/*

$sql2 = "INSERT INTO endpointtable (ename, id, data) VALUES ('$user_name', 51, '$end_user_ip')";

if (mysqli_query($conn, $sql2)){
  $logs="New record created successfully";
} else {
  $logs="ERROR";
}
*/
//$conn->close()

mysqli_close($conn);


#mysqli_close($conn);

//$url="http://192.168.115.130:8000/restconf/config/sc/nsf/firewall/policy/testPolicy/".$jsond;
//header('Location:'.$url);

//Connect to a local server //
#$host = "127.0.0.1";
#$TCP_PORT = 6000;
#$output= "enterprise-mode,create,".$result;
#$socket = socket_create(AF_INET, SOCK_STREAM,0);
#socket_connect ($socket , $host,$TCP_PORT );
#socket_write($socket, $output, strlen ($output));
#socket_close($socket);
header( "refresh:3;url=enterprise.php" );
// Creating a text log file //

//$date = date_create("NOW");
//$file = 'log.txt';
//$test = date_format($date,"Y/m/d H:i:s") . '-' . $_POST["Rule_name"] . '-' . $_POST["Position"] . '-' . $_POST["Website"] . '-' . $_POST["Starting_Time"] . '-' . $_POST["Ending_Time"] . '-' . $_POST["Action"] . "\n";
//$ret = file_put_contents($file, $test, FILE_APPEND | LOCK_EX);


//header("refresh:0;url=qfc2.php/api/Policy_web");
  

?>
