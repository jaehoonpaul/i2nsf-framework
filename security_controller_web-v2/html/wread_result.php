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

$Web = $_GET["web_mode"];
$Rule_name = $_GET["Mode"];

$arr = explode(":", $Web, 3);
$Rule_id = $arr[1];
$arr = explode("Rule_name", $Rule_id, 2);
$Rule_id = $arr[0];
$Rule_id = str_replace(' ','', $Rule_id);


$sql1="SELECT * FROM Policy_web WHERE Rule_id='$Rule_id';";

$result1 = mysql_query($sql1) or die ("Query to get data from Policy_web failed: ".mysql_error());
$array = array(); // make a new array to hold all your data
while($row = mysql_fetch_assoc($result1)){ // loop to store the data in an associative array.
$array[] = $row['Rule_name'];

}
$row1 = $array[0];


$sql2="select Position FROM Policy_web WHERE Rule_id='$Rule_id';";
$result2 = mysql_query($sql2) or die ("Query to get data from Policy_web failed: ".mysql_error());
$array2 = array(); // make a new array to hold all your data
while($row2 = mysql_fetch_assoc($result2)){ // loop to store the data in an associative array.
$array2[] = $row2['Position'];

}
$row2 = $array2[0];


$sql3="select Web FROM Policy_web WHERE Rule_id='$Rule_id';";
$result3 = mysql_query($sql3) or die ("Query to get data from Policy_web failed: ".mysql_error());
$array3 = array(); // make a new array to hold all your data
while($row3 = mysql_fetch_assoc($result3)){ // loop to store the data in an associative array.
$array3[] = $row3['Web'];

}
$row3 = $array3[0];


$sql4="select Start_time FROM Policy_web WHERE Rule_id='$Rule_id';";
$result4 = mysql_query($sql4) or die ("Query to get data from Policy_web failed: ".mysql_error());
$array4 = array(); // make a new array to hold all your data
while($row4 = mysql_fetch_assoc($result4)){ // loop to store the data in an associative array.
$array4[] = $row4['Start_time'];

}
$row4 = $array4[0];


$sql5="select End_time FROM Policy_web WHERE Rule_id='$Rule_id';";
$result5 = mysql_query($sql5) or die ("Query to get data from Policy_web failed: ".mysql_error());
$array5 = array(); // make a new array to hold all your data
while($row5 = mysql_fetch_assoc($result5)){ // loop to store the data in an associative array.
$array5[] = $row5['End_time'];

}
$row5 = $array5[0];


$sql6="select Action FROM Policy_web WHERE Rule_id='$Rule_id';";
$result6 = mysql_query($sql6) or die ("Query to get data from Policy_web failed: ".mysql_error());
$array6 = array(); // make a new array to hold all your data
while($row6 = mysql_fetch_assoc($result6)){ // loop to store the data in an associative array.
$array6[] = $row6['Action'];

}
$row6 = $array6[0];


//$url="http://192.168.115.130:8000/restconf/config/sc/nsf/firewall/policy/testPolicy/".$jsond;
//header('Location:'.$url);

//echo $jsond;

mysql_close($conn);

?>
<div class = "center">
<p><span class="error">* required field.</span></p>
<form method="post" id = "form" action=""> 
  <span class="error">* <?php echo $idErr;?></span>
  Rule ID:
  <input type="text" name = "Rule_id" id = "Rule id" value = "<?php echo $Rule_id;?>" readonly>
  Rule Name: <a id = "img"><img src='qsm.png'></a>
  <div id = "image">This field is where you enter the name of your policy to distinguish it from others.</div>
  <input type="text" name = "Rule_name" id = "Rule name" value = "<?php echo $row1;?>" readonly>
  <br><br>
  <span class="error">* <?php echo $posErr; ?></span>
  Position: <a id = "img"><img src='qsm.png'></a>
  <div id = "image">Here, you specify the relevent position of your employee who you want to restrict the access to the below websites.</div>
  <select name="Position" id = "Position" readonly
onFocus='this.initialSelect = this.selectedIndex;'
onChange='this.selectedIndex = this.initialSelect;'>
  <option value="select"<?=($row4 == "select")? "selected='selected'" : "" ?>>Select</option>
  <option value="President" <?=($row2 == "President")? "selected='selected'" : "" ?>>President</option>
  <option value="Vice President" <?=($row2 == "Vice_President")? "selected='selected'" : "" ?>>Vice President</option>
  <option value="Senior Managing Director" <?=($row2 == "Senior Managing Director")? "selected='selected'" : "" ?>>Senior Managing Director</option>
  <option value="Managing Director"<?=($row2 == "Managing Director")? "selected='selected'" : "" ?>>Managing Director</option>
  <option value="Department Manager" <?=($row2 == "Department Manager")? "selected='selected'" : "" ?>>Department Manager</option>
  <option value="Manager" <?=($row2 == "Manager")? "selected='selected'" : "" ?>>Manager</option>
  <option value="Assistant Manager" <?=($row2 == "Assistant Manager")? "selected='selected'" : "" ?>>Assistant Manager</option>
  <option value="Staff" <?=($row2 == "Staff")? "selected='selected'" : "" ?>>Staff</option>
  </select>
  <br><br>
  <span class="error">* <?php echo $webErr; ?></span>
  Website: <a id = "img"><img src='qsm.png'></a>
  <div id = "image">Here, you can select the name of a website which you want to block.</div>
  <input type="text" name = "Website" id = "Website" value = "<?php echo $row3;?>" readonly>
  <br><br>
  <span class="error">* <?php echo $stimeErr; ?></span>
  Starting Time : <a id = "img"><img src='qsm.png'></a>
  <div id = "image">For Starting Time and Ending Time field, you can decide when to begin and end the restriction for the website you've chosen from above. </div>
  <select name = "Starting_Time" id="Starting Time" readonly
onFocus='this.initialSelect = this.selectedIndex;'
onChange='this.selectedIndex = this.initialSelect;'>
  <option value="select"<?=($row4 == "select")? "selected='selected'" : "" ?>>Select</option>
  <option value="01:00"<?=($row4 == "01:00")? "selected='selected'" : "" ?>>01:00</option>
  <option value="02:00"<?=($row4 == "02:00")? "selected='selected'" : "" ?>>02:00</option>
  <option value="03:00"<?=($row4 == "03:00")? "selected='selected'" : "" ?>>03:00</option>
  <option value="04:00"<?=($row4 == "04:00")? "selected='selected'" : "" ?>>04:00</option>
  <option value="05:00"<?=($row4 == "05:00")? "selected='selected'" : "" ?>>05:00</option>
  <option value="06:00"<?=($row4 == "06:00")? "selected='selected'" : "" ?>>06:00</option>
  <option value="07:00"<?=($row4 == "07:00")? "selected='selected'" : "" ?>>07:00</option>
  <option value="08:00"<?=($row4 == "08:00")? "selected='selected'" : "" ?>>08:00</option>
  <option value="09:00"<?=($row4 == "09:00")? "selected='selected'" : "" ?>>09:00</option>
  <option value="10:00"<?=($row4 == "10:00")? "selected='selected'" : "" ?>>10:00</option>
  <option value="11:00"<?=($row4 == "11:00")? "selected='selected'" : "" ?>>11:00</option>
  <option value="12:00"<?=($row4 == "12:00")? "selected='selected'" : "" ?>>12:00</option>
  <option value="13:00"<?=($row4 == "13:00")? "selected='selected'" : "" ?>>13:00</option>
  <option value="14:00"<?=($row4 == "14:00")? "selected='selected'" : "" ?>>14:00</option>
  <option value="15:00"<?=($row4 == "15:00")? "selected='selected'" : "" ?>>15:00</option>
  <option value="16:00"<?=($row4 == "16:00")? "selected='selected'" : "" ?>>16:00</option>
  <option value="17:00"<?=($row4 == "17:00")? "selected='selected'" : "" ?>>17:00</option>
  <option value="18:00"<?=($row4 == "18:00")? "selected='selected'" : "" ?>>18:00</option>
  <option value="19:00"<?=($row4 == "19:00")? "selected='selected'" : "" ?>>19:00</option>
  <option value="20:00"<?=($row4 == "20:00")? "selected='selected'" : "" ?>>20:00</option>
  <option value="21:00"<?=($row4 == "21:00")? "selected='selected'" : "" ?>>21:00</option>
  <option value="22:00"<?=($row4 == "22:00")? "selected='selected'" : "" ?>>22:00</option>
  <option value="23:00"<?=($row4 == "23:00")? "selected='selected'" : "" ?>>23:00</option>
  <option value="00:00"<?=($row4 == "00:00")? "selected='selected'" : "" ?>>00:00</option>
  </select>
  <br><br>
  <span class="error">* <?php echo $etimeErr;?></span>
  Ending Time :
  <select name = "Ending_Time" id="Ending Time" readonly
onFocus='this.initialSelect = this.selectedIndex;'
onChange='this.selectedIndex = this.initialSelect;'>
  <option value="select"<?=($row4 == "select")? "selected='selected'" : "" ?>>Select</option>
  <option value="01:00"<?=($row5 == "01:00")? "selected='selected'" : "" ?>>01:00</option>
  <option value="02:00"<?=($row5 == "02:00")? "selected='selected'" : "" ?>>02:00</option>
  <option value="03:00"<?=($row5 == "03:00")? "selected='selected'" : "" ?>>03:00</option>
  <option value="04:00"<?=($row5 == "04:00")? "selected='selected'" : "" ?>>04:00</option>
  <option value="05:00"<?=($row5 == "05:00")? "selected='selected'" : "" ?>>05:00</option>
  <option value="06:00"<?=($row5 == "06:00")? "selected='selected'" : "" ?>>06:00</option>
  <option value="07:00"<?=($row5 == "07:00")? "selected='selected'" : "" ?>>07:00</option>
  <option value="08:00"<?=($row5 == "08:00")? "selected='selected'" : "" ?>>08:00</option>
  <option value="09:00"<?=($row5 == "09:00")? "selected='selected'" : "" ?>>09:00</option>
  <option value="10:00"<?=($row5 == "10:00")? "selected='selected'" : "" ?>>10:00</option>
  <option value="11:00"<?=($row5 == "11:00")? "selected='selected'" : "" ?>>11:00</option>
  <option value="12:00"<?=($row5 == "12:00")? "selected='selected'" : "" ?>>12:00</option>
  <option value="13:00"<?=($row5 == "13:00")? "selected='selected'" : "" ?>>13:00</option>
  <option value="14:00"<?=($row5 == "14:00")? "selected='selected'" : "" ?>>14:00</option>
  <option value="15:00"<?=($row5 == "15:00")? "selected='selected'" : "" ?>>15:00</option>
  <option value="16:00"<?=($row5 == "16:00")? "selected='selected'" : "" ?>>16:00</option>
  <option value="17:00"<?=($row5 == "17:00")? "selected='selected'" : "" ?>>17:00</option>
  <option value="18:00"<?=($row5 == "18:00")? "selected='selected'" : "" ?>>18:00</option>
  <option value="19:00"<?=($row5 == "19:00")? "selected='selected'" : "" ?>>19:00</option>
  <option value="20:00"<?=($row5 == "20:00")? "selected='selected'" : "" ?>>20:00</option>
  <option value="21:00"<?=($row5 == "21:00")? "selected='selected'" : "" ?>>21:00</option>
  <option value="22:00"<?=($row5 == "22:00")? "selected='selected'" : "" ?>>22:00</option>
  <option value="23:00"<?=($row5 == "23:00")? "selected='selected'" : "" ?>>23:00</option>
  <option value="00:00"<?=($row5 == "00:00")? "selected='selected'" : "" ?>>00:00</option>
  </select>
  <br><br>
  <span class="error">* <?php echo $actErr;?></span>
  Action: <a id = "img"><img src='qsm.png'></a>
  <div id = "image">Here, you can choose either to block or allow access to the website you've chosen.</div>
  <select name="Action" id = "Action" readonly
onFocus='this.initialSelect = this.selectedIndex;'
onChange='this.selectedIndex = this.initialSelect;'>
  <option value="select"<?=($row4 == "select")? "selected='selected'" : "" ?>>Select</option>
  <option value="alert"<?=($row6 == "alert")? "selected='selected'" : "" ?>>alert</option>
  <option value="reject"<?=($row6 == "reject")? "selected='selected'" : "" ?>>reject</option>
  <option value="drop"<?=($row6 == "drop")? "selected='selected'" : "" ?>>drop</option>
  <option value="pass"<?=($row6 == "pass")? "selected='selected'" : "" ?>>pass</option>
  </select>
  <br><br>
<input type="button" value="OK" class = "ok button" id = "button_ok" onClick="location.href='select_page.php'">
</form>
</div>
</body>
</html>

