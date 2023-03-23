import React from 'react';
import { useState } from 'react';
import './group-forms.css'

function Devicegroupsform() {


    // variables for the IPv4 and IPv6 dropdowns    
    const [moreIPv4, setMoreIPv4] = useState(false);
    const [moreIPv6, setMoreIPv6] = useState(false);



    // variables for the registration validation 
    const [form, setForm] = useState({'name':null,"mac-address":null,"range-ipv4-address":{"start":null,"end":null},"range-ipv6-address":{"start":null,"end":null}});


    const handleSubmit = (e) => {
        e.preventDefault()
        
        console.log(form)
        
    }



  return (
    <div style={{margin:"20px"}}>
        {/* form to submit the user-group part */}
        <form> 
            <div className='nameInput'>
                {/* Name label and Input */}
                <label htmlFor="name">Name: </label>
                <input name="name" id="name" type="text" value= {form.nameInput}
                onChange={(e) => form['name'] = e.target.value}
                autoComplete='off'
                placeholder='ENTER DEVICE GROUP NAME'/>
            </div>

            <br></br>

            <div className='macAddressInput'>
                {/* Mac Address label (the second part) the form */}
                <label htmlFor='macAddress'>Mac Address: </label>
                <input name="Mac Address" id='macAddress' autoComplete='off' placeholder='ENTER MAC ADDRESS' value= {form.macAddressInput}
                 onChange={(e) => form['mac-address'] = e.target.value} />        
            </div>

             <br></br>

            {/* radio buttons part  */}

            <h2 style={{textAlign:"center"}}>IP type</h2>

            <div className='ipvs'>
                {/*Selection of IP version (IPv4 or IPv6) */}
                <tr>
                    <td>
                        <label htmlFor="ipv4">IPv4</label> 
                        <input name='ipv' id="ipv4" type="radio" value="ipv4" onClick={() => {setMoreIPv4(true);  setMoreIPv6(false);
                        form['range-ipv6-address']['start'] = null; form['range-ipv6-address']['end'] = null}} />
                    </td>
                    <td>
                        <label htmlFor="ipv6">IPv6</label>
                        <input name='ipv' id="ipv6" type="radio" value="ipv6" onClick={() => {setMoreIPv6(true) ; setMoreIPv4(false); 
                        form['range-ipv4-address']['start'] = null; 
                        form['range-ipv4-address']['end'] = null}}/>
                    </td>
                </tr>

                <br></br>

                {/* hidden more ipv4 information (when upper button is clicked) */}

                <div className={moreIPv4 ? "isShown" : "isHidden"}>
                    <label htmlFor="ipv4Start">Start-IPv4-address: </label>
                    <input type="text" id='ipv4Start' value = {form.start} onChange={(e) => form['range-ipv4-address']['start'] = e.target.value}
                    autoComplete='off'/>

                    <br></br>
                    <br></br>

                    <label htmlFor="ipv4End">End-IPv4-address: </label>
                    <input type="text" id='ipv4End' value = {form.end} onChange={(e) => form['range-ipv4-address']['end'] = e.target.value} autoComplete='off' />
                </div>
                
                {/* hidden more ipv6 information (when upper button is clicked) */}

                <div className={moreIPv6 ? "isShown" : "isHidden"}>
                    <label htmlFor="ipv6Start">Start-IPv6-address: </label>
                    <input type="text" id="ipv6Start"                       
                    value = {form.end}
                    autoComplete='off'
                    onChange={(e) => form['range-ipv6-address']['start'] = e.target.value}/>
                    
                    <br></br>
                    <br></br>

                    <label htmlFor="ipv6End">End-IPv6-address: </label>
                    <input type="text" id='ipv6End'                     
                    value = {form.end}
                    autoComplete='off'
                    onChange={(e) => form['range-ipv6-address']['end'] = e.target.value} />
                </div>

            </div>

            <br></br>
            <br></br>

            <footer className='footer'>
                <button type='submit' onClick={handleSubmit}>Submit</button>
            </footer>
        </form>
    </div>
  )
}

export default Devicegroupsform;