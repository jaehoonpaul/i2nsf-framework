const express = require("express");
const app = express();
const port = 3001; // <- 3000에서 다른 숫자로 변경
const cors = require("cors");
const bodyParser = require("body-parser");
const mysql = require("mysql"); // << 새로 추가된 부분

var connection = mysql.createConnection({
  /// 새로 추가된 부분
  host: "10.0.0.17",
  user: "root", // mysql에 아이디를 넣는다.
  password: "secu", // mysql의 비밀번호를 넣는다.
  database: 'monitoring', //위에서 만든 데이터베이스의 이름을 넣는다.
});

connection.connect();

app.use(bodyParser.urlencoded({ extended: false }));
app.use(cors());
app.use(bodyParser.json());

app.get("/", (req, res) => {
  res.send("Hello World!");
});

// old value
app.get("/urldataold", (req, res) => {
  connection.query("SELECT * FROM `i2nsf-system-res-util-log` WHERE `nsf-name` = 'url_filtering' ORDER BY eventTime DESC limit 2", function (err, rows, fields) {
    if (err) {
      console.log("fail");
    } else {
      console.log(rows);
      res.send(rows);
    }
  });
});

// new
app.get("/urldata", (req, res) => {
  connection.query("SELECT * FROM `i2nsf-system-res-util-log` WHERE `nsf-name` = 'url_filtering' ORDER BY eventTime DESC limit 1", function (err, rows, fields) {
    if (err) {
      console.log("fail");
    } else {
      console.log(rows[0]);
      res.send(rows[0]);
    }
  });
});

// new
app.get("/timedata", (req, res) => {
  connection.query("SELECT * FROM `i2nsf-system-res-util-log` WHERE `nsf-name` = 'time_based_firewall'  ORDER BY eventTime DESC limit 1", function (err, rows, fields) {
    if (err) {
      console.log("fail");
    } else {
      console.log(rows[0]);
      res.send(rows[0]);
    }
  });
});

//old value
app.get("/timedataold", (req, res) => {
  connection.query("SELECT * FROM `i2nsf-system-res-util-log` WHERE `nsf-name` = 'time_based_firewall'  ORDER BY eventTime DESC limit 2", function (err, rows, fields) {
    if (err) {
      console.log("fail");
    } else {
      console.log(rows);
      res.send(rows);
    }
  });
});


app.listen(port, () => {
  console.log(`Example app listening at http://localhost:${port}`);
});
