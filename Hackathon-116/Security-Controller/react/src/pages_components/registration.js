import React from 'react'
import { useState } from 'react';
import Usermodal from '../modals_components/usermodal';
import Devicemodal from '../modals_components/devicemodal';
import Locationmodal from '../modals_components/locationmodal';
import Urlmodal from '../modals_components/urlmodal';
import './registration.css'



export default function Registration(props) {

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

        <button className={props.mode === 'dark' ? 'dark-button' : 'light-button'} style={{marginTop:"100px"}} onClick={() => {setOpenModal(true);}}>User Groups</button>
        {openModal && <Usermodal closeModal={setOpenModal} mode={props.mode}/>} 

        <button className={props.mode === 'dark' ? 'dark-button' : 'light-button'} onClick={() => {setOpenDeviceModal(true);}}>Device Groups</button>
        {openDeviceModal && <Devicemodal closeDeviceModal={setOpenDeviceModal} mode={props.mode}/>}

        <button className={props.mode === 'dark' ? 'dark-button' : 'light-button'}  onClick={() => {setOpenLocationModal(true);}}>Location Groups</button>
        {openLocationModal && <Locationmodal closeLocationModal={setOpenLocationModal} mode={props.mode}/>}

        <button className={props.mode === 'dark' ? 'dark-button' : 'light-button'} onClick={() => {setOpenUrlModal(true);}}>URL Groups</button>
        {openUrlModal && <Urlmodal closeUrlModal={setOpenUrlModal} mode={props.mode}/>}
        
    </div>
  )
}
