
import React from 'react';
import Locationgroupsform from './location-group-forms';
import "./modal.css";



function Locationmodal({closeLocationModal,mode}) {
  return (
    <div className='modalBackground'>
        <div className={mode === 'dark' ? 'dark-modalContainer' : 'light-modalContainer'}>
            <button className='closeModalBtn' onClick={() => {closeLocationModal(false);} }> X </button>
            <div className='title'>
              <h1>Location Group</h1>
            </div>
            <div className='body'>
                <Locationgroupsform />
            </div>
        </div>
    </div>
  )
}

export default Locationmodal