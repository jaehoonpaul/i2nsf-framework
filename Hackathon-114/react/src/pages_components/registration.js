import React from 'react'
import { useState } from 'react';
import Usermodal from '../modals_components/usermodal';
import Devicemodal from '../modals_components/devicemodal';
import Locationmodal from '../modals_components/locationmodal';
import Urlmodal from '../modals_components/urlmodal';
import './registration.css'



export default function Registration() {

  // variables for user group modal
  const [openModal, setOpenModal] = useState(false);
  
  // variables for device group modal
  const [openDeviceModal, setOpenDeviceModal] = useState(false)

  // variables for the location group modal
  const [openLocationModal, setOpenLocationModal] = useState(false)

  // variables for the URL group modal

  const [openUrlModal, setOpenUrlModal] = useState(false)


  return (
    <div className='registration'>

        <button className='openModalBtn' onClick={() => {setOpenModal(true);}}>User Groups</button>
        {openModal && <Usermodal closeModal={setOpenModal} />} 

        <button className='openModalBtn2' onClick={() => {setOpenDeviceModal(true);}}>Device Groups</button>
        {openDeviceModal && <Devicemodal closeDeviceModal={setOpenDeviceModal} />}

        <button className='openModalBtn2'  onClick={() => {setOpenLocationModal(true);}}>Location Groups</button>
        {openLocationModal && <Locationmodal closeLocationModal={setOpenLocationModal} />}

        <button className='openModalBtn2' onClick={() => {setOpenUrlModal(true);}}>Url Groups</button>
        {openUrlModal && <Urlmodal closeUrlModal={setOpenUrlModal} />}
        
    </div>
  )
}
