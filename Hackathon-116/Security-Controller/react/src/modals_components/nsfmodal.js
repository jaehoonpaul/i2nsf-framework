import React from 'react';
import "./modal.css";
import styled from "styled-components";

const Spacediv = styled.div`
  white-space: pre-wrap;
  justify-content: left;
  align-items: left;
  text-align:left;
`;

const Title = styled.h1`
  font-size: 1.5em;
  text-align: center;
`;

const Wrapper = styled.section`
  padding: 4em;
  background: papayawhip;
`;

const TableWrapper = styled.div`
  max-height: 50vh; /* adjust this to your desired max height */
  overflow-y: auto;
  margin-top: 1em;
`;

function numberWithCommas(x) {
  return x.toString().replace(/\B(?=(\d{3})+(?!\d))/g, ",");
}

function Nsfmodal({closeModal,data,mode}) {
  console.log(data)
  
  const addRow = (key, value) => {
    rows.push(
      <tr key={key}>
        <td style={{textAlign:"right"}}>{key}</td>
        <td style={{textAlign:"left"}}>{value}</td>
      </tr>
    );
  };
  var re = /[0-9]/;
  const getLeafNodes = (obj, prefix) => {
    for (const [key, value] of Object.entries(obj)) {
      if (typeof value === 'object') {
        getLeafNodes(value, prefix + key + '/');
      } 
      else {
        if (re.test(key)) {
          addRow(prefix , value);
        }
        else {
          addRow(prefix + key, value);
        }
        
      }
    }
  };
  
  const rows = [];
  getLeafNodes(data, '');
  return (
      <div className='modalBackground' >
        <div className={mode === 'dark' ? 'dark-modalContainer' : 'light-modalContainer'} >
            <button className='closeModalBtn' onClick={() => {closeModal(false);} }> X </button>
            <Title>
              {data["nsf-name"]}
            </Title>
            <TableWrapper> 
            <table style={{borderSpacing: "20px"}}>
              <tbody>
                {rows}
                {/* <tr>
                  <td>Name</td> <td>{data["nsf-name"]}</td>
                </tr>
                <tr>
                  <td>Version</td> <td>{data["version"]}</td>
                </tr>
                <tr>
                  <td>IP</td> <td>{data["nsf-access-info"]["ip"]}</td>
                </tr>
                <tr>
                  <td>Protocol</td> <td>{data["nsf-access-info"]["management-protocol"]}</td>
                </tr>
                <tr>
                  <td>Port</td> <td>{data["nsf-access-info"]["port"]}</td>
                </tr>
                <tr>
                  <td>CPU</td> <td>{data["nsf-specification"]["cpu"]["model"]}</td>
                </tr>
                <tr>
                  <td>Memory</td> <td>8192 MB</td>
                </tr>
                <tr>
                  <td>Bandwidth</td> <td>{numberWithCommas(data["nsf-specification"]["bandwidth"]["inbound"]/1000000)} MBps</td>
                </tr>
                <tr>
                  <td>Event-Capabilities</td> <td>{"event-capabilities" in data ? 
                                                          "yes": "NaN"}</td>
                </tr>
                <tr>
                  <td>Condition-Capabilities</td> <td>{"condition-capabilities" in data 
                                                        ? "generic-nsf-capabilities" in data["condition-capabilities"] 
                                                          ? "ipv4-capability" in data["condition-capabilities"]["generic-nsf-capabilities"] ? data["condition-capabilities"]["generic-nsf-capabilities"]["ipv4-capability"]: "NaN"
                                                          : "NaN"
                                                        : "NaN"}</td>
                </tr> */}
              </tbody>
            </table>
            </TableWrapper>
        </div>
      </div>

  )
}

export default Nsfmodal