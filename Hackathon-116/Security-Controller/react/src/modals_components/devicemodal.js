import React from 'react';
import Devicegroupsform from './device-group-forms';
import "./modal.css";



function Devicemodal({closeDeviceModal,mode}) {
  return (
    <div className='modalBackground'>
        <div className={mode === 'dark' ? 'dark-modalContainer' : 'light-modalContainer'}>
            <button className='closeModalBtn' onClick={() => {closeDeviceModal(false);} }> X </button>
            <div className='title'>
              <h1>Registration</h1>
            </div>
            <div className='body'>
                <Devicegroupsform />
            </div>
        </div>
    </div>
  )
}

export default Devicemodal