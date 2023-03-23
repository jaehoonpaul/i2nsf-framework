import React from 'react';
import Urlgroupsform from './url-group-forms';
import "./modal.css";

function Urlmodal({closeUrlModal,mode}) {
  return (
      <div className='modalBackground'>
          <div className={mode === 'dark' ? 'dark-modalContainer' : 'light-modalContainer'}>
              <button className='closeModalBtn' onClick={() => {closeUrlModal(false);} }> X </button>
              <div className='title'>
                <h1>URL Groups</h1>
              </div>
              <div className='body'>
                  <Urlgroupsform mode={mode}/>
              </div>
          </div>
      </div>
  )
}

export default Urlmodal