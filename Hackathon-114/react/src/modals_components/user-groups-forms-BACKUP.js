import React from 'react';
import { useState } from 'react';
import './user-group-forms.css'


function Usergroupsform() {


    // variables for the IPv dropdown buttons
    const [moreIPv4, setMoreIPv4] = useState(false);
    const [moreIPv6, setMoreIPv6] = useState(false);


    // variables for the registration validation 
    const [form, setForm] = useState({'name':null,"mac-address":null,"range-ipv4-address":{"start":null,"end":null},"range-ipv6-address":{"start":null,"end":null}});
    

    
    // variables and functions to handle the submit button and connect to MONGODB

    

    const handleSubmit = (e) => {
        e.preventDefault()
        
        console.log(form)
        
    }







  return (
    <div>
    
        {/* form to submit the user-group part */}
        <form> 

            <div className='nameInput'>
            
                {/* Name label and Input */}
                <label htmlFor="name">Name: </label>
                <input name="name" id="name" type="text" 
                placeholder='ENTER USER GROUP NAME' 
                value= {form.nameInput}
                onChange={(e) => form['name'] = e.target.value}
                autoComplete="off"
                />

            </div>

                <br></br>

            <div className='macAddressInput'>
                
                {/* Mac Address label (the second part) the form */}
                <label htmlFor='macAddress'>Mac Address: </label>
                <input 
                id='macAddress'
                name="Mac Address"
                placeholder='ENTER MAC ADDRESS'
                value= {form.macAddressInput}
                onChange={(e) => form['mac-address'] = e.target.value} 
                autoComplete="off"/>
                        
            </div>

                <br></br>

            {/* radio buttons part  */}
          <h2>IP type</h2>

        <div className='ipvs'>
         
            <label htmlFor="ipv4">IPv4</label> 
            <input 
            name='ipv' 
            id="ipv4"
            type="radio" 
            value="ipv4" 
            onClick={() => {setMoreIPv4(true); 
                            setMoreIPv6(false);
                            form['range-ipv6-address']['start'] = null; 
                            form['range-ipv6-address']['end'] = null}}
           />
            
            
            <br></br>
            <br></br>
            
            {/* hidden more ipv4 information (when upper button is clicked) */}
            <div className={moreIPv4 ? "isShown" : "isHidden"}>

                <div className='ipv4-start'>\

                    <label htmlFor="ipv4Start">Start-IPv4-address: </label>
                    <input type="text" id='ipv4Start'
                    value = {form.start}
                    onChange={(e) => form['range-ipv4-address']['start'] = e.target.value}
                    autoComplete='off'/>

                 </div>

                <br></br>

                <div className='ipv4-end'>

                    <label htmlFor="ipv4End">End-IPv4-address: </label>
                    <input type="text" id='ipv4End'
                    value = {form.end}
                    onChange={(e) => form['range-ipv4-address']['end'] = e.target.value} 
                    autoComplete='off'/>

                 </div>

                
            </div>

            <br></br>
            <br></br>
            

            <label htmlFor="ipv6">IPv6</label>
            <input name='ipv' 
            id="ipv6" 
            type="radio" 
            value="ipv6" 
            onClick={() => {setMoreIPv6(true);
                            setMoreIPv4(false); 
                            form['range-ipv4-address']['start'] = null; 
                            form['range-ipv4-address']['end'] = null}}
            />
            


            <br></br>
            <br></br>
            
            {/* hidden more ipv6 information (when upper button is clicked) */}
            <div className={moreIPv6 ? "isShown" : "isHidden"}>

                <div className='ipv6-start'>

                    <label htmlFor="ipv6Start">Start-IPv6-address: </label>
                    <input type="text" id="ipv6Start" 
                     value = {form.end}
                     autoComplete='off'
                     onChange={(e) => form['range-ipv6-address']['start'] = e.target.value}/>

                </div>

                <br></br>
                <br></br>

                <div className='ipv6-end'>

                    <label htmlFor="ipv6End">End-IPv6-address: </label>
                    <input type="text" id='ipv6End' 
                     value = {form.end}
                     autoComplete='off'
                     onChange={(e) => form['range-ipv6-address']['end'] = e.target.value}/>

                </div>
            </div>
        </div>


        <footer className='footer'>

            <button type='submit' onClick={handleSubmit}>Submit</button>
            <button id='cancelBtn'>Cancel</button>

        </footer>
            

        </form>

    </div>
  )
}

export default Usergroupsform;