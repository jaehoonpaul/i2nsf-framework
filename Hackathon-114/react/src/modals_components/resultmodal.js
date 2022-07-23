import React from 'react';
import Usergroupsform from './user-groups-forms';
import "./usermodal.css";
import styled from "styled-components";



const Spacediv = styled.div`
  white-space: pre-wrap;
  justify-content: left;
  align-items: left;
  text-align:left;
`;
function PrintXML(data) {
    let array =[]
        for (let i =0; i < Object.keys(data).length; i++)
            {
                array.push(
                <Spacediv>
                {Object.keys(data)[i]}<br></br>
                {Object.values(data)[i]}<br></br>
                </Spacediv>
                )
            }
        
            return array
}

function Resultmodal({closeModal,data}) {
    console.log(data)

  return (
    <div className='modalBackground'>
        <div className='modalContainer'>
            <button className='closeModalBtn' onClick={() => {closeModal(false);} }> X </button>
            <div className='title'>
              <h1>Translated Low-Level Policy</h1>
            </div>
            {PrintXML(data)}
            
        </div>
    </div>
  )
}

export default Resultmodal