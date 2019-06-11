<?php
function array_to_xml( $data, &$xml_data ) {
    foreach( $data as $key => $value ) {
        if( is_array($value) ) {
            if( is_numeric($key) ){
                $key = 'Policy_web';
            }
            $subnode = $xml_data->addChild($key);
            array_to_xml($value, $subnode);
        } else {
            $xml_data->addChild($key, htmlspecialchars($value));
        }
    }
}

$rule_name = $_POST['rulename'];
$rule_case = 'web';
$start_time = $_POST['starttime'];
$end_time = $_POST['endtime'];
$source = $_POST['src'];
$destination = $_POST['dst'];
$action = $_POST['act'];

$container = array();
$ruleData = array();
#$ruleData["rule-id"] = $Rule_id;
$ruleData["rule-name"] = $rule_name;
$timeinfo = array("start-time" => $start_time, "end-time" => $end_time);
$eventData = array();
$eventData["time-information"] = [$timeinfo];
$ruleData["event"] = [$eventData];
$condData = [array("rule-case" => $rule_case, "source" => $source, "destination" => $destination)];
$ruleData["condition"] = $condData;
$actData = [array("action-name" => $action)];
$ruleData["action"] = $actData;
$groupData["i2nsf:rule"] = array();
$groupData["i2nsf:rule"] = $ruleData;
file_put_contents("test.json", json_encode($groupData, JSON_PRETTY_PRINT));

$CLIENT_CERT = "/works/jetconf/data/example-client.pem";
$POST_DATA = "@test.json";
$URL = "https://localhost:8443/restconf/data/i2nsf:Policy";
$log = shell_exec("curl --ipv4 --http2 -k --cert-type PEM -E $CLIENT_CERT -X POST -d $POST_DATA $URL");

// Creating an XML File //
$data_str = file_get_contents('test.json');
 
$xml_data = new SimpleXMLElement('<?xml version="1.0"?><I2NSF></I2NSF>');
array_to_xml(json_decode($data_str, true), $xml_data);
$result = $xml_data -> asXML();
 
header('Content-Type: text/xml; charset=UTF-8');
print_r($result);


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
header( "refresh:3;url=select_page.php" );
// Creating a text log file //

//$date = date_create("NOW");
//$file = 'log.txt';
//$test = date_format($date,"Y/m/d H:i:s") . '-' . $_POST["Rule_name"] . '-' . $_POST["Position"] . '-' . $_POST["Website"] . '-' . $_POST["Starting_Time"] . '-' . $_POST["Ending_Time"] . '-' . $_POST["Action"] . "\n";
//$ret = file_put_contents($file, $test, FILE_APPEND | LOCK_EX);


//header("refresh:0;url=qfc2.php/api/Policy_web");
  

?>
