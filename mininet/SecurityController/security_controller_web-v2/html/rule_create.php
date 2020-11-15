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
<script src="https://code.jquery.com/jquery-3.5.0.js"></script>

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
<p><span class="error">* required field.</span></p>
<form method="post" id = "form" action="create_result.php"> 
 <fieldset id = "Policy">
  <legend>Policy</legend>
   <label>Policy name</label><br><input type="text" name="policyname" /><br>
   <label>Rule name</label><br><input type="text" name="rulename" /><br>
  <fieldset id = "condition">
   <legend>Condition</legend>
   <label>Source Target</label>
    <input type="text" name="src" value=""><br>
   <label>Destination Target</label><br>
    <input type="text" name="dst" value=""><br>
   <label>Start time (YYYY-MM-DDThh:mm:ssZ, ex: 2020-07-20T09:00:00Z)</label>
    <input type="text" name="starttime" value=""><br>
<!--    
<select name="starttime" id="starttime">
	<option value="">Select...</option>
    	<option value="01:00">01:00</option>
	<option value="02:00">02:00</option>
	<option value="03:00">03:00</option>
	<option value="04:00">04:00</option>
	<option value="05:00">05:00</option>
	<option value="06:00">06:00</option>
	<option value="07:00">07:00</option>
	<option value="08:00">08:00</option>
	<option value="09:00">09:00</option>
	<option value="10:00">10:00</option>
	<option value="11:00">11:00</option>
	<option value="12:00">12:00</option>
	<option value="13:00">13:00</option>
	<option value="14:00">14:00</option>
	<option value="15:00">15:00</option>
	<option value="16:00">16:00</option>
	<option value="17:00">17:00</option>
	<option value="18:00">18:00</option>
	<option value="19:00">19:00</option>
	<option value="20:00">20:00</option>
	<option value="21:00">21:00</option>
	<option value="22:00">22:00</option>
	<option value="23:00">23:00</option>
	<option value="00:00">00:00</option>
	</select><br>
-->
   <label>End time (YYYY-MM-DDThh:mm:ssZ, ex: 2020-07-20T18:00:00Z)</label>
	<input type="text" name="endtime" value=""><br>
<!--
    <select name="endtime" id="endtime">
	<option value="">Select...</option>
	<option value="01:00">01:00</option>
	<option value="02:00">02:00</option>
	<option value="03:00">03:00</option>
	<option value="04:00">04:00</option>
	<option value="05:00">05:00</option>
	<option value="06:00">06:00</option>
	<option value="07:00">07:00</option>
	<option value="08:00">08:00</option>
	<option value="09:00">09:00</option>
	<option value="10:00">10:00</option>
	<option value="11:00">11:00</option>
	<option value="12:00">12:00</option>
	<option value="13:00">13:00</option>
	<option value="14:00">14:00</option>
	<option value="15:00">15:00</option>
	<option value="16:00">16:00</option>
	<option value="17:00">17:00</option>
	<option value="18:00">18:00</option>
	<option value="19:00">19:00</option>
	<option value="20:00">20:00</option>
	<option value="21:00">21:00</option>
	<option value="22:00">22:00</option>
	<option value="23:00">23:00</option>
	<option value="00:00">00:00</option>
	</select>
-->
   <label>Frequency</label> <br>
   <select name="frequency" id="frequency">
	<option value="only-once">only-once</option>
        <option value="daily">daily</option>
	<option value="weekly">weekly</option>
        <option value="monthly">monthly</option>
	<option value="yearly">yearly</option>
   </select><br>
<label id="Day">Day <br>
        <label><input type="checkbox" value="monday">Monday</label>
        <label><input type="checkbox" value="tuesday">Tuesday</label>
        <label><input type="checkbox" value="wednesday">Wednesday</label>
        <label><input type="checkbox" value="thursday">Thursday</label>
        <label><input type="checkbox" value="friday">Friday</label>
	<label><input type="checkbox" value="saturday">Saturday</label>
	<label><input type="checkbox" value="sunday">Sunday</label>
</label>
<label id="Date">Date (ex: 1,2,3,10,31)
    <input type="text" name="extra" />
</label>
<label id="Month">Month (MM-DD ex: 07-23,08-23,12-22)
    <input type="text" name="extra" />
</label>
<script>
$("#Day").hide();
$("#Date").hide();
$("#Month").hide();

$("#frequency").change(function(){
        if( $(this).val() == "weekly"){
                $("#Day").show();
                $("#Date").hide();
                $("#Month").hide();
        }else if( $(this).val() == "monthly") {
                $("#Date").show();
                $("#Day").hide();
                $("#Month").hide();
        }else if( $(this).val() == "yearly") {
        	$("#Month").show();
        	$("#Date").hide();
                $("#Day").hide();
        }else{
		$("#Day").hide();
		$("#Date").hide();
		$("#Month").hide();
	}
});
</script>
</fieldset>
 <fieldset id = "action">
   <label>Actions<label><br></br>
   <select name="act" id="act">
	<option value="">Select...</option>
	<option value="pass">Pass</option>
	<option value="drop">Drop</option>
        <option value="alert">Alert</option>
        <option value="rate-limit">Rate-limit</option>
        <option value="mirror">Mirror</option>
	</select>
  </fieldset><br>
<input type="submit" name = "submit" value="Submit"/>
</form>
</div>
</body>
</html>


