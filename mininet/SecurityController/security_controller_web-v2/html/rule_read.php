<TYPE HTML>
<html>
<head>
<style>
.center {
    margin: left;
    width: 90%;
    border: 3px solid green;
    padding: 10px;
}
input[type=text], select {
    width: 100%;
    padding: 12px 20px;
    margin: 8px 0;
    display: inline-block;
    border: 1px solid #ccc;
    border-radius: 4px;
    box-sizing: border-box;
}

input[type=button] {
    width: 100%;
    background-color: #4CAF50;
    color: white;
    padding: 12px 20px;
    margin: 8px 0;
    border: none;
    border-radius: 4px;
    cursor: pointer;
}

input[type=button]:hover {
    background-color: #00FF00;
}

select {
    width: 100%;
    padding: 16px 20px;
    border: none;
    border-radius: 4px;
    background-color: white;
}


div {
    border-radius: 5px;
    background-color: #f2f2f2;
    padding: 20px;
}
#img img{

  max-width: 20px; 
  max-height: 20px;
}

#image {
    display: none;
    border: 0px solid green;
    background-color: #F0E68C;
    max-height:50px;
    max-width:400px;
    margin-top: 10px;
}

a:hover + #image {
    display: block;
}

.error {color: #FF0000;}

</style>
</head>
<body>
<div>

<?php
$servername="localhost";
$username="root";
$password="secu";
$conn=mysql_connect(localhost, $username, $password) or die ("Error connecting to mysql server: ".mysql_error());
            
$dbname = 'I2NSF';
mysql_select_db($dbname, $conn) or die ("Error selecting specified database on mysql server: ".mysql_error());

$sql="SELECT Rule_id, Rule_name FROM Policy_enterprise";
$result=mysql_query($sql) or die ("Query to get data from firsttable failed: ".mysql_error());

#$CLIENT_CERT = "/works/jetconf/data/example-client.pem";
#$URL = "https://localhost:8443/restconf/data/i2nsf:policy/I2NSF/Policy_enterprise";
#$results = shell_exec("curl --ipv4 --http2 -k --cert-type PEM -E $CLIENT_CERT -X GET $URL");
#$json = json_decode($result, true);

echo "<form action=\"\" method=\get\">";
echo "Enterprise_Mode";
echo "<select name=\"enterprise_mode\">";
while ($row=mysql_fetch_array($result)) {
$Rule_id = $row['Rule_id'];
$Rule_name=$row['Rule_name'];
echo "<option>
Rule_id:$Rule_id
Rule_name:$Rule_name
</option>";
 }
echo "</select>";

?>

<input type="button" value="OK" class = "ok button" id = "button_ok" onClick="location.href='enterprise.php'"/>
</div>
</form>
</body>
</html>


