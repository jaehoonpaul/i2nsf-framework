import React from 'react'
import { useState, useEffect } from 'react';
import './NSFs.css'
import Nsfmodal from '../modals_components/nsfmodal';

function numberWithCommas(x) {
  return x.toString().replace(/\B(?=(\d{3})+(?!\d))/g, ",");
}

export default function NSFs({mode}) {
  
  const [openModal, setOpenModal] = useState(false);

  const [posts, setPosts] = useState([]);

  const [value, setValue] = useState([]);

  useEffect(() => {
    fetch('http://115.145.178.185:5000/nsfDB/get')
       .then((response) => response.json())
       .then((data) => {
          setPosts(data["nsf"]);
       })
       .catch((err) => {
          console.log(err.message);
       });
  }, []);

  console.log(posts)

  return (
    <div className='NSFs' style={{height:"100vh"}}>
      <h1>Registered NSFs</h1>
      <table className='nsf-table'>
        <tbody>
          <tr>
            <th>Name</th>
            <th>Version</th>
            <th>Access Information</th>
            <th>Specification</th>
          </tr>
          {posts.map((val, key) => {
            const isEvenRow = key % 2 === 0;
            const rowStyle = {
              backgroundColor: isEvenRow
                ? mode === 'dark' ? '#444444' : '#f2f2f2'
                : mode === 'dark' ? '#333333' : '#e6e6e6',
              borderBottom: 'thin solid #000000'
            };
            return (
              <tr key={key} style={rowStyle}>
                <td style={{borderBottom:"thin solid #000000"}} ><label className="nsfname" style={{color: mode === 'dark' ? '#00BFFF' : '#0070FF'}} onClick={(e) => {setOpenModal(true);setValue(val)}}>{val["nsf-name"]}</label></td>
                <td style={{borderBottom:"thin solid #000000"}}>{val["version"]}</td>
                <td style={{borderBottom:"thin solid #000000"}}>
                  <table style={{ margin: "auto",border:0}}>
                    <tbody>
                    <tr style={{textAlign:"right"}}>
                      IP:
                      <td >
                        {val["nsf-access-info"]["ip"]}
                      </td>
                    </tr>
                    <tr style={{textAlign:"right"}}>
                      Protocol: 
                      <td>
                        {val["nsf-access-info"]["management-protocol"]}
                      </td>
                    </tr>
                    <tr style={{textAlign:"right"}}>
                      Port: 
                      <td>
                        {val["nsf-access-info"]["port"]}
                      </td>
                    </tr>
                    </tbody>
                  </table> 
                </td>
                <td style={{borderBottom:"thin solid #000000"}}>
                <table style={{ margin: "auto",border:0}}>
                    <tr style={{textAlign:"right"}}>
                      CPU:
                      <td>
                        {val["nsf-specification"]["cpu"]["model"]}
                      </td>
                    </tr>
                    <tr style={{textAlign:"right"}}>
                      Memory: 
                      <td>
                      8192 MB
                      </td>
                    </tr>
                    <tr style={{textAlign:"right"}}>
                      Bandwidth:
                      <td>
                      {numberWithCommas(val["nsf-specification"]["bandwidth"]["inbound"]/1000000)} MBps
                      </td>
                    </tr>
                  </table> 
                </td>
              </tr>
            )
          })}
        </tbody>
      </table>
      {openModal && <Nsfmodal closeModal={setOpenModal} mode={mode} data={value}/>} 
    </div>
    
  )
}
