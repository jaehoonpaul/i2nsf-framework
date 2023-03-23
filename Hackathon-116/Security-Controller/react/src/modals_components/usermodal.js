import React from 'react';
import Usergroupsform from './user-groups-forms';
import "./modal.css";

import styled from "styled-components";

const Title = styled.h1`
  font-size: 1.5em;
  text-align: center;
`;


function Usermodal({closeModal,mode}) {
  return (
    <div className='modalBackground'>
        <div className={mode === 'dark' ? 'dark-modalContainer' : 'light-modalContainer'}>
            <button className='closeModalBtn' onClick={() => {closeModal(false);} }> X </button>
            <Title>User Group Registration</Title>
            <div className='body'>
                <Usergroupsform mode={mode}/>
            </div>
        </div>
    </div>
  )
}

export default Usermodal