<?php
function array_to_xml( $data, &$xml_data ) {
    foreach( $data as $key => $value ) {
        if( is_array($value) ) {
            if( !is_numeric($key) ){
		$subnode = $xml_data->addChild($key);
            	array_to_xml($value, $subnode);
            } else{
	        array_to_xml($value, $xml_data);
	    }
        } else {
            $subnode = $xml_data->addChild("$key","$value");
        }
	if(strpos($key, 'i2nsf-cfi-policy') == True){
		$subnode->addAttribute('xmlns', 'urn:ietf:params:xml:ns:yang:ietf-i2nsf-cfi-policy');
	}
    }
}

function formatXml($simpleXMLElement){
	$xmlDocument = new DOMDocument('1.0');
	$xmlDocument->preserveWhiteSpace = false;
	$xmlDocument->formatOutput = true;
	$xmlDocument->loadXML($simpleXMLElement->asXML());

	return $xmlDocument->saveXML();
}

$user_name = $_POST['userName'];
$start_user_ip = $_POST['startUserIP'];
$end_user_ip = $_POST['endUserIP'];
$device_name = $_POST['deviceName'];
$start_dev_ip = $_POST['startDevIP'];
$end_dev_ip = $_POST['endDevIP'];

$userIP = array("start-ipv4-address" => $start_user_ip, "end-ipv4-address" => $end_user_ip);
$policyName = "Registration-Policy";
$userInfo = array("name" => $user_name, "range-ipv4-address" => $userIP);
$userGroup = $userInfo;
#$devIP = array("start-ipv4-address" => $start_dev_ip, "end-ipv4-address" => $end_dev_ip);
#$devInfo = array("name" => $device_name, "range-ipv4-address" => [$devIP]);
#$devGroup = $devInfo;
$endGroup = array("user-group" => [$userGroup]);
$groupData["ietf-i2nsf-cfi-policy:i2nsf-cfi-policy"] = [array("policy-name" => $policyName, "endpoint-groups" => $endGroup)];
file_put_contents("regi_ip.json", json_encode($groupData, JSON_PRETTY_PRINT));

$CLIENT_CERT = "/home/ubuntu/works/jetconf/data/example-client.pem";

//$userIP = array("start-ip-address" => $start_user_ip, "end-ip-address" => $end_user_ip);
//$userInfo = array("name" => $user_name, "range-ip-address" => [$userIP]);
//$userGroup = $userInfo;
//$devIP = array("start-ip-address" => $start_dev_ip, "end-ip-address" => $end_dev_ip);
//$devInfo = array("name" => $device_name, "range-ip-address" => [$devIP]);
//$devGroup = $devInfo;
//$endGroup["endpoint-group"] = array("user-group" => [$userGroup], "device-group" => [$devGroup]);
//$groupData["ietf-i2nsf-cfi-policy:policy"] = $endGroup;
//file_put_contents("regi.json", json_encode($groupData, JSON_PRETTY_PRINT));

//$CLIENT_CERT = "/home/ubuntu/works/jetconf/data/example-client.pem";
$POST_DATA = "@regi_ip.json";
$URL = "https://localhost:8443/restconf/data/ietf-i2nsf-cfi-policy:i2nsf-cfi-policy";
$log = shell_exec("/usr/local/bin/curl --ipv4 --http2 -k --cert-type PEM -E $CLIENT_CERT -X PUT -d $POST_DATA $URL");

// Creating an XML File //
$data_str = file_get_contents('regi_ip.json'); 
$xml_data = new SimpleXMLElement('<?xml version="1.0"?><config></config>');

array_to_xml(json_decode($data_str, true), $xml_data);
header('Content-Type: text/xml; charset=UTF-8');


$xmlContent = formatXml($xml_data);
print_r($log);
file_put_contents("/home/ubuntu/XML/regi_ip.xml",$xmlContent);
//$result = $xml_data -> asXML('/home/ubuntu/XML/regi_ip2.xml');
$dbname = "nsfdb";

$conn = mysqli_connect("127.0.0.1","root","secu",$dbname);

if (!$conn) {
  die("Connection failed: " . mysqli_connect_error());
}

$sql = "INSERT INTO endpointtable (ename, id, data) VALUES ('$user_name', 43, '$start_user_ip')";

if (mysqli_query($conn, $sql)) {
  $logs="New record created successfully";
} else {
  $logs="ERROR";
}


$sql2 = "INSERT INTO endpointtable (ename, id, data) VALUES ('$user_name', 44, '$end_user_ip')";

if (mysqli_query($conn, $sql2)){
  $logs="New record created successfully";
} else {
  $logs="ERROR";
}

//$conn->close()

mysqli_close($conn);

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
