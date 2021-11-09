<TYPE HTML>
<html>
<head>
<script>

</script>

<style>
.center {
    margin: auto;
    width: 100%;

}
input[type=button] {
    width: 90%;
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


div {
    border-radius: 5px;
    background-color: #f2f2f2;
    padding: 20px;
}
</style>
</head>
<body>


<div class = "center">
<form>
<input type="button" name="submit_button" value="Registration" onclick="location.href='regi_select.php'"/>
<input type="button" name="submit_button" value="Rule Create" onclick="location.href='rule_create.php'"/>
<!--
<input type="button" name="submit_button" value="Rule Update" onclick="location.href='rule_update.php'"/>
<input type="button" name="submit_button" value="Rule Read" onclick="location.href='rule_read.php'"/>
<input type="button" name="submit_button" value="Rule Delete" onclick="location.href='rule_delete.php'"/>
-->
</form>
</div>


</body>
</html>


