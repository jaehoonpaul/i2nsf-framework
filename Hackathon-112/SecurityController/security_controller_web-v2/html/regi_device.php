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

input[type=submit] {
    width: 100%;
    background-color: #4CAF50;
    color: white;
    padding: 12px 20px;
    margin: 8px 0;
    border: none;
    border-radius: 4px;
    cursor: pointer;
}

input[type=submit]:hover {
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

<?php
// $idErr = $posErr = $webErr = $stimeErr = $etimeErr = $actErr = "";
// $id = $pos = $act = $web = $stime = $etime = "";


function test_input($data) {
  $data = trim($data);
  $data = stripslashes($data);
  $data = htmlspecialchars($data);
  return $data;

}

?>
<div class = "center">
<p><span class="error">Registration field</span></p>
<form method="post" id = "form" action="regi_result2.php"> 
<!--
 <fieldset id = "usergroup">
  <legend>User registration</legend>
   <label>User name</label><br><input type="text" name="userName" /><br>
   <label>Range of user IP<br>
   <label>--Start IP address--</label><br><input type="text" name="startUserIP" /><br>
   <label>--End IP address--</label><br><input type="text" name="endUserIP" /><br>
 </fieldset>
-->
  <fieldset id = "devicegroup">
   <legend>Device registration</legend>
   <label>Device name</label>
    <input type="text" name="deviceName" value=""><br>
   <label>Range of device IP</label><br>
   <label>--Start IP address--</label><br><input type="text" name="startDevIP" /><br>
   <label>--End IP address--</label><br><input type="text" name="endDevIP" /><br>
 </fieldset>

<!--
 <fieldset id = "protocol">
   <label>protocol<label><br></br>
   <select name="act" id="act">
	<option value="">Select...</option>
	<option value="Pass">Pass</option>
	<option value="Reject">Reject</option>
	<option value="Log">Log</option>
	</select>
  </fieldset><br>
-->
<input type="submit" name = "submit" value="Submit"/>
</form>
</div>
</body>
</html>


