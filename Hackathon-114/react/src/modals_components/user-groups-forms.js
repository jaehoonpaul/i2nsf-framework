import React from 'react';
import { useState } from 'react';
import './user-group-forms.css'


function Usergroupsform() {


    // variables for the ipv dropdown buttons
    const [moreIPv4, setMoreIPv4] = useState(false);
    const [moreIPv6, setMoreIPv6] = useState(false);


    // variables for the registration validation 
    const [form, setForm] = useState({'name':null,"mac-address":null,"range-ipv4-address":{"start":null,"end":null},"range-ipv6-address":{"start":null,"end":null}});
    const [errors, setErrors] = useState({});

    
    // variables and functions to handle the submit button and connect to MONGODB

    function validateIPv4 (value) {
        var re = /^(?:(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.){3}(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)$/;
        if (re.test(value)) {
            return true;
        }
        else {
            alert("Wrong IPv4 Format");
            return false;
        }
    }

    function validateIPv6 (value) {
        var re = /(([0-9a-fA-F]{1,4}:){7,7}[0-9a-fA-F]{1,4}|([0-9a-fA-F]{1,4}:){1,7}:|([0-9a-fA-F]{1,4}:){1,6}:[0-9a-fA-F]{1,4}|([0-9a-fA-F]{1,4}:){1,5}(:[0-9a-fA-F]{1,4}){1,2}|([0-9a-fA-F]{1,4}:){1,4}(:[0-9a-fA-F]{1,4}){1,3}|([0-9a-fA-F]{1,4}:){1,3}(:[0-9a-fA-F]{1,4}){1,4}|([0-9a-fA-F]{1,4}:){1,2}(:[0-9a-fA-F]{1,4}){1,5}|[0-9a-fA-F]{1,4}:((:[0-9a-fA-F]{1,4}){1,6})|:((:[0-9a-fA-F]{1,4}){1,7}|:)|fe80:(:[0-9a-fA-F]{0,4}){0,4}%[0-9a-zA-Z]{1,}|::(ffff(:0{1,4}){0,1}:){0,1}((25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])\.){3,3}(25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])|([0-9a-fA-F]{1,4}:){1,4}:((25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])\.){3,3}(25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9]))/;
        if (re.test(value)) {
            return true;
        }
        else {
            alert("Wrong IPv6 Format");
            return false;
        }
    }

    function validateMAC (value) {
        var re = /^([0-9A-Fa-f]{2}[:-]){5}([0-9A-Fa-f]{2})$/;
        if (re.test(value)) {
            return true;
        }
        else {
            alert("Wrong MAC Address Format");
            return false;
        }
    }

    const handleSubmit = async (e) => {
        e.preventDefault()
        var mac = document.getElementById("macBox")
        var startipv4 = document.getElementById("startIPv4")
        var endipv4 = document.getElementById("endIPv4")
        var startipv6 = document.getElementById("startIPv6")
        var endipv6 = document.getElementById("endIPv6")

        let checkForm = true;
        
        if (form["mac-address"]!=null) {
            if (!validateMAC(form["mac-address"])) {
                mac.style.color = "red";
                checkForm = false;
            }
            else{
                mac.style.color = "black";
            }
        }

        if (form["range-ipv4-address"]["start"]!=null){
            if (!validateIPv4(form["range-ipv4-address"]["start"])){
                startipv4.style.color = "red";
                checkForm = false;
            }
            else {
                startipv4.style.color = "black";
            }
        }
        if (form["range-ipv4-address"]["end"]!=null) {
            if (!validateIPv4(form["range-ipv4-address"]["end"])){
                endipv4.style.color = "red";
                checkForm = false;
            }
            else {
                endipv4.style.color = "black";
            }
        }

        if (form["range-ipv6-address"]["start"]!=null) {
            if (!validateIPv6(form["range-ipv6-address"]["start"])) {
                startipv6.style.color = "red";
                checkForm = false;
            }
            else {
                startipv6.style.color = "black";
            }
        }
        if (form["range-ipv6-address"]["end"]!=null) {
            if (!validateIPv6(form["range-ipv6-address"]["end"])) {
                endipv6.style.color = "red";
                checkForm = false;
            }
            else {
                endipv6.style.color = "black";
            }
        }

        if (checkForm){
            const response = await fetch('http://localhost:5000/user/put', {
                method: 'PUT',
                body: JSON.stringify(form),
                headers: {
                    'Content-Type': 'application/json'
                }
            })
            
            const myJson = await response.text();
            alert(myJson)
            //console.log(myJson)
        }
        

        
    }

  return (
    <div>
    
        {/* form to submit the user-group part */}
        <form> 

            <div className='nameInput'>
            
                {/* Name label and Input */}
                <label for="name">Name: </label>
                <input name="name" id="name" type="text" 
                placeholder='ENTER USER GROUP NAME' 
                value= {form.nameInput}
                onChange={(e) => form['name'] = e.target.value}
                onInvalid={!!errors.nameInput}
                />

            </div>

                <br></br>

            <div className='macAddressInput' id="macBox">
                
                {/* Mac Address label (the second part) the form */}
                <label>Mac Address: </label>
                <input name="Mac Address"
                 placeholder='ENTER MAC ADDRESS'
                 value= {form.macAddressInput}
                 onChange={(e) => form['mac-address'] = e.target.value} />
                        
            </div>

                <br></br>

            {/* radio buttons part  */}
          <h2>IP type</h2>

        <div className='ipvs'>
         
            <label for="ipv4">IPv4</label> 
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

                <div className='ipv4-start' id="startIPv4">
                    <label for="ipv4Start">Start-IPv4-address: </label>
                    <input type="text" id='ipv4Start'
                     value = {form.start}
                     onChange={(e) => {  
                                        if (e.target.value === ''){
                                            form['range-ipv4-address']['start'] = null
                                        }
                                        else{
                                            form['range-ipv4-address']['start'] = e.target.value
                                        }
                                      }
                     }/>
                 </div>

                <br></br>
                <br></br>

                <div className='ipv4-end' id="endIPv4">
                    <label for="ipv4End">End-IPv4-address: </label>
                    <input type="text" is='ipv4End'
                    value = {form.end}
                    onChange={(e) => {  
                                        if (e.target.value === ''){
                                            form['range-ipv4-address']['end'] = null
                                        }
                                        else{
                                            form['range-ipv4-address']['end'] = e.target.value
                                        }
                                     }
                    } />
                 </div>

                
            </div>

            <br></br>
            <br></br>
            

            <label for="ipv6">IPv6</label>
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

                <div className='ipv6-start' id="startIPv6">
                    <label for="ipv6Start">Start-IPv6-address: </label>
                    <input type="text" id="ipv6Start" 
                     value = {form.end}
                     onChange={(e) => {  
                                        if (e.target.value === ''){
                                            form['range-ipv6-address']['start'] = null
                                        }
                                        else{
                                            form['range-ipv6-address']['start'] = e.target.value
                                        }
                                      }
                     }/>
                </div>

            <br></br>
            <br></br>

                <div className='ipv6-end' id="endIPv6">
                    <label for="ipv6End">End-IPv6-address: </label>
                    <input type="text" id='ipv6End' 
                     value = {form.end}
                     onChange={(e) => {  
                                        if (e.target.value === ''){
                                            form['range-ipv6-address']['end'] = null
                                        }
                                        else{
                                            form['range-ipv6-address']['end'] = e.target.value
                                        }
                                      }
                    }/>
                </div>
            </div>
        </div>

        <br></br>
        <br></br>


        <footer className='footer'>
            <button type='submit' onClick={handleSubmit}>Submit</button>
            <button id='cancelBtn'>Cancel</button>
        </footer>
            

        </form>

    </div>
  )
}

export default Usergroupsform;
