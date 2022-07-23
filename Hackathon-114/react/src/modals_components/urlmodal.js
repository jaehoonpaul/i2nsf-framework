import React from 'react';
import Urlgroupsform from './url-group-forms';
import "./urlmodal.css";



function Urlmodal({closeUrlModal}) {
  return (
    <div className='modalBackground'>
        <div className='modalContainer'>
            <button className='closeModalBtn' onClick={() => {closeUrlModal(false);} }> X </button>
            <div className='title'>
              <h1>Url Groups</h1>
            </div>
            <div className='body'>
                <Urlgroupsform />
            </div>
        </div>
    </div>
  )
}

export default Urlmodal