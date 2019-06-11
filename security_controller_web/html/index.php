<?php 
session_start();

define('DS', TRUE); // used to protect includes
define('USERNAME', $_SESSION['username']);
define('SELF', $_SERVER['PHP_SELF'] );

if (!USERNAME or isset($_GET['logout']))
 include('login.php');




// everything below will show after correct login 
?>
