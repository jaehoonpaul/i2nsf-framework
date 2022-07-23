import React from 'react';
import Usergroupsform from './user-groups-forms';
import "./usermodal.css";



function Usermodal({closeModal}) {


  return (
    <div className='modalBackground'>
        <div className='modalContainer'>
            <button className='closeModalBtn' onClick={() => {closeModal(false);} }> X </button>
            <div className='title'>
              <h1>Registration</h1>
            </div>
            <div className='body'>
                <Usergroupsform />
            </div>
        </div>
    </div>
  )
}

export default Usermodal