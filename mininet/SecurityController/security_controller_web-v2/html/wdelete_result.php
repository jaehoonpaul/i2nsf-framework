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

$servername="localhost";
$username="root";
$password="secu";
$conn=mysql_connect(localhost, $username, $password) or die ("Error connecting to mysql server: ".mysql_error());
            
$dbname = 'I2NSF';
mysql_select_db($dbname, $conn) or die ("Error selecting specified database on mysql server: ".mysql_error());

$Web = $_GET["web_mode"];
$Rule_name = $_GET["Mode"];

$arr = explode(":", $Web, 3);
$Rule_id = $arr[1];
$arr = explode("Rule_name", $Rule_id, 2);
$Rule_id = $arr[0];
$Rule_id = str_replace(' ','', $Rule_id);


$result = mysql_query("SELECT * FROM Policy_web WHERE Rule_id=$Rule_id");

$rows = array();
#while($r = mysql_fetch_assoc($result)){

#$rows = $r;
#}
$Rule_id = mysql_fetch_row($result)[0];

$sql1="SELECT * FROM Policy_web WHERE Rule_id='$Rule_id';";

$result1 = mysql_query($sql1) or die ("Query to get data from Policy_web failed: ".mysql_error());
$array = array(); // make a new array to hold all your data
while($row = mysql_fetch_assoc($result1)){ // loop to store the data in an associative array.
$array[] = $row['Rule_name'];

}
$Rule_name = $array[0];


$sql2="select Position FROM Policy_web WHERE Rule_id='$Rule_id';";
$result2 = mysql_query($sql2) or die ("Query to get data from Policy_web failed: ".mysql_error());
$array2 = array(); // make a new array to hold all your data
while($row2 = mysql_fetch_assoc($result2)){ // loop to store the data in an associative array.
$array2[] = $row2['Position'];

}
$Position = $array2[0];

$sql3="select Web FROM Policy_web WHERE Rule_id='$Rule_id';";
$result3 = mysql_query($sql3) or die ("Query to get data from Policy_web failed: ".mysql_error());
$array3 = array(); // make a new array to hold all your data
while($row3 = mysql_fetch_assoc($result3)){ // loop to store the data in an associative array.
$array3[] = $row3['Web'];

}
$Web = $array3[0];

$sql4="select Start_time FROM Policy_web WHERE Rule_id='$Rule_id';";
$result4 = mysql_query($sql4) or die ("Query to get data from Policy_web failed: ".mysql_error());
$array4 = array(); // make a new array to hold all your data
while($row4 = mysql_fetch_assoc($result4)){ // loop to store the data in an associative array.
$array4[] = $row4['Start_time'];

}
$Start_time = $array4[0];

$sql5="select End_time FROM Policy_web WHERE Rule_id='$Rule_id';";
$result5 = mysql_query($sql5) or die ("Query to get data from Policy_web failed: ".mysql_error());
$array5 = array(); // make a new array to hold all your data
while($row5 = mysql_fetch_assoc($result5)){ // loop to store the data in an associative array.
$array5[] = $row5['End_time'];

}
$End_time = $array5[0];

$sql6="select Action FROM Policy_web WHERE Rule_id='$Rule_id';";
$result6 = mysql_query($sql6) or die ("Query to get data from Policy_web failed: ".mysql_error());
$array6 = array(); // make a new array to hold all your data
while($row6 = mysql_fetch_assoc($result6)){ // loop to store the data in an associative array.
$array6[] = $row6['Action'];

}
$Action = $array6[0];

//$url="http://192.168.115.130:8000/restconf/config/sc/nsf/firewall/policy/testPolicy/".$jsond;
//header('Location:'.$url);

//echo $jsond;


$container = array();
$ruleData = array();
$ruleData["rule-id"] = $Rule_id;
$ruleData["rule-name"] = $Rule_name;
$ruleData["rule-case"] = "web";
$timeinfo = array("start-time" => $Start_time, "end-time" => $End_time);
$eventData = array();
$eventData["time-information"] = [$timeinfo];
$ruleData["event"] = [$eventData];
$condData = [array("source" => $Position, "destination" => $Web)];
$ruleData["condition"] = $condData;
$actData = [array("action-name" => $Action)];
$ruleData["action"] = $actData;
$groupData["i2nsf:rule"] = array();
$groupData["i2nsf:rule"] = $ruleData;
file_put_contents("test.json", json_encode($groupData, JSON_PRETTY_PRINT));


$CLIENT_CERT = "/works/jetconf/data/example-client.pem";

$POST_DATA='{ "jetconf:input": {"name": "Edit 1", "options": "config"} }';
$URL = "https://127.0.0.1:8443/restconf/operations/jetconf:conf-start";
$log = shell_exec("curl --ipv4 --http2 -k --cert-type PEM -E $CLIENT_CERT -X POST -d $POST_DATA $URL");

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

#$host = "127.0.0.1";
#$TCP_PORT = 6000;
#$output= "web,delete,".$result;
#$socket = socket_create(AF_INET, SOCK_STREAM,0);
#socket_connect ($socket , $host,$TCP_PORT );
#socket_write($socket, $output, strlen ($output));
#socket_close($socket);


//$url="http://192.168.115.130:8000/restconf/config/sc/nsf/firewall/policy/testPolicy/".$jsond;
//header('Location:'.$url);

//echo $jsond;
sleep(1);
$updatesql="DELETE FROM Policy_web WHERE Rule_id='$Rule_id';";
mysql_query($updatesql) or die("Query to delete record in Policy_enterprise failed with this error: ".mysql_error());

mysql_close($conn);

header( "refresh:1;url=select_page.php" );

?>


