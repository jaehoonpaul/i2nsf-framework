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
    }
}

function formatXml($simpleXMLElement){
        $xmlDocument = new DOMDocument();
        $xmlDocument->preserveWhiteSpace = false;
        $xmlDocument->formatOutput = true;
        $xmlDocument->loadXML($simpleXMLElement->asXML());

        return $xmlDocument->saveXML($xmlDocument->documentElement);
}

$policy_name = $_POST['policyname'];
$rule_name = $_POST['rulename'];
$source = $_POST['src'];
$destination = $_POST['dst'];
$action = $_POST['act'];
$start_time = $_POST['starttime'];
$end_time = $_POST['endtime'];
#$action = "";
#if(!empty($_POST['action'])) {
#	foreach($_POST['action'] as $item) {
#		$action .= $item.',';
#	}
#}
$policyData = array();
$eventData = array();
$groupData = array();
$container = array();
$ruleData = array();
$group = array();

$ruleData["rule-name"] = $rule_name;
$src_target["src-target"] = $source;
$fire_con["source-target"] = $src_target;
$dest_target["dest-target"] = $destination;
$cust_con["destination-target"] = $dest_target;
$condData = array("firewall-condition" => $fire_con, "custom-condition" => $cust_con);
$ruleData["condition"] = $condData;
$timeinfo = array("begin-time" => $start_time, "end-time" => $end_time);
$eventData["time-information"] = $timeinfo;
$ruleData["event"] = $eventData;
$actData["primary-action"] = $action;
$ruleData["action"] = $actData;
$groupData = array("policy-name" => $policy_name, "rule" =>  [$ruleData]);
file_put_contents("test.json", json_encode($groupData, JSON_PRETTY_PRINT));

$CLIENT_CERT = "/home/ubuntu/works/jetconf/data/example-client.pem";
$POST_DATA = "@test.json";
$URL = "https://localhost:8443/restconf/data/ietf-i2nsf-cfi-policy:i2nsf-cfi-policy";
$log = shell_exec("curl --ipv4 --http2 -k --cert-type PEM -E $CLIENT_CERT -X POST -d $POST_DATA $URL");

//print_r(json_encode($groupData, JSON_PRETTY_PRINT));

// Creating an XML File //
$data_str = file_get_contents('test.json');

$xml_data = new SimpleXMLElement('<ietf-i2nsf-cfi-policy:policy></ietf-i2nsf-cfi-policy:policy>');

array_to_xml(json_decode($data_str, true), $xml_data);
header('Content-Type: text/xml; charset=UTF-8');

$xmlContent = formatXml($xml_data);

file_put_contents("/home/ubuntu/XML/rule.xml",$xmlContent);
$log2 = shell_exec("cp /home/ubuntu/XML/rule.xml /home/ubuntu/HighLevelPolicy/rule.txt");
shell_exec("python /home/ubuntu/send.py");

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
