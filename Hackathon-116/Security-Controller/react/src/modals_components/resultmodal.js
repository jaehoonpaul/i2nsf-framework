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

function PrintXML(data) {
  console.log(data)
    let array =[]
      for (let i =0; i < Object.keys(data).length; i++) {
        array.push(
          <Spacediv>
            {Object.keys(data)[i]}<br></br>
            {Object.values(data)[i]}<br></br>
          </Spacediv>
        )
      }
      return array
}

function Resultmodal({closeModal,data,mode}) {
  console.log(data)
  return (
      <div className='modalBackground'>
        <div className={mode === 'dark' ? 'dark-modalContainer' : 'light-modalContainer'}>
            <button className='closeModalBtn' onClick={() => {closeModal(false);} }> X </button>
            <Title>
              Translated Low-Level Policy
            </Title>
            <div style = {{ marginLeft : 20, marginRight: 20}} className='content'>
              <h3 style={{fontFamily:"Courier New"}}>{PrintXML(data)}</h3>
            </div>
        </div>
      </div>

  )
}

export default Resultmodal