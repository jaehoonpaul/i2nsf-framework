<?php
$servername = "localhost";
$username = "root";
$password = "secu";
$dbname = "I2NSF";

$conn = new mysqli($servername, $username, $password);

if ($conn->connect_error) {
    die("Connection failed: " . $conn->connect_error);
} 


if ($conn->connect_error) {
    die("Connection failed: " . $conn->connect_error);
} 

$sql = "CREATE DATABASE I2NSF";
if ($conn->query($sql) === TRUE) {
    echo "I2NSF Database created successfully";
} else {
    echo "Error creating database: " . $conn->error;
}
$conn->close();

$conn = new mysqli($servername, $username, $password, $dbname);

$sql = $sql = "CREATE TABLE Policy_enterprise (
Rule_id INT(6) UNSIGNED AUTO_INCREMENT PRIMARY KEY, 
Rule_name VARCHAR(30), 
Event_id INT(6) UNSIGNED, 
Event_name VARCHAR(50), 
Condition_id INT(6) UNSIGNED, 
Conditions VARCHAR(50), 
User_group VARCHAR(50), 
Device_group VARCHAR(50), 
Action VARCHAR(20)
)";

if (mysqli_query($conn, $sql)){
	echo "Table Policy_enterprise created successfully";
}else {
	echo "Error creating table: " .mysqli_error($conn);
}

$sql = $sql = "CREATE TABLE Policy_web (
Rule_id INT(6) UNSIGNED AUTO_INCREMENT PRIMARY KEY,
Rule_name VARCHAR(30) NOT NULL,
Position VARCHAR(50) NOT NULL,
Web VARCHAR(50) Not NULL,
Start_time VARCHAR(5) NOT NULL,
End_time VARCHAR(5) NOT NULL,
Action VARCHAR(10) NOT NULL
)";

if (mysqli_query($conn, $sql)){
	echo "Table Policy_Web created successfully";
}else {
	echo "Error creating table: " .mysqli_error($conn);
}

$conn->close();
?>
