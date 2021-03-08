<?php defined('DS') OR die('No direct access allowed.');



define('DS', TRUE);

$users = array(
 "admin" => "secu"
);

if(isset($_GET['logout'])) {
    $_SESSION['username'] = '';
    header('Location:  ' . $_SERVER['PHP_SELF']);
}

if(isset($_POST['username'])) {
    if($users[$_POST['username']] !== NULL && $users[$_POST['username']] == $_POST['password']) {
  $_SESSION['username'] = $_POST['username'];
  header("Location: enterprise.php");
  die();
    }else {

  echo "<p>error logging in</p>";
    }
}



?>


<!DOCTYPE html>
<html>
<style>

form {
    border: 3px solid #f1f1f1;
}

input[type=text], input[type=password] {
    width: 100%;
    padding: 12px 20px;
    margin: 8px 0;
    display: inline-block;
    border: 1px solid #ccc;
    box-sizing: border-box;
}

input[type = submit] {
    background-color: #4CAF50;
    color: white;
    padding: 14px 20px;
    margin: 8px 0;
    border: none;
    cursor: pointer;
    width: 100%;
}

.cancelbtn {
    width: auto;
    padding: 20px 18px;
    background-color: #f44336;
}



.container {
    padding: 16px;
}

span.psw {
    float: right;
    padding-top: 16px;
}

/* Change styles for span and cancel button on extra small screens */
@media screen and (max-width: 300px) {
    span.psw {
       display: block;
       float: none;
    }
 
</style>

<body>

<h2>Admin Login Page</h2>
<?php echo '
<form method="post" action="'.SELF.'">
<div class="container">
  <label for="username"><b>Username</b></label>
  <input type="text" placeholder="Enter Username" id="username" name="username" value="" />
  <label for="password"><b>Password</b></label>
  <input type="password" placeholder = "Enter Password" id="password" name="password" value="" />
  <input type="submit" name="submit" value="Login" class="button"/>

</div>
</form>';
exit;
?>

</body>
</html>



