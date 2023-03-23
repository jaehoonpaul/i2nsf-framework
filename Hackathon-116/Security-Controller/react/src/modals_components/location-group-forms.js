import React from 'react';
import { useState } from 'react';
import './group-forms.css'

function Locationgroupsform() {
    // variables for the IPv4 and IPv 6 drop down
    const [locationIPv4, setLocationIPv4] = useState(true);
    const [locationIPv6, setLocationIPv6] = useState(false);


    // variables for the registration validation 
    const [form, setForm] = useState({'country':null, 'region':null, 'city':null, "geo-ipv4-address":{"start":null,"end":null},"geo-ipv6-address":{"start":null,"end":null}});


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



    const handleSubmit = async (e) => {
      e.preventDefault()

      var startlocationipv4 = document.getElementById("startlocationipv4")
      var endlocationipv4 = document.getElementById("endlocationipv4")
      var startlocationipv6 = document.getElementById("startlocationipv6")
      var endlocationipv6 = document.getElementById("endlocationipv6")
      
      console.log(form)

      let checkForm = true;

      if (form["geo-ipv4-address"]["start"]!=null){
        if (!validateIPv4(form["geo-ipv4-address"]["start"])){
            startlocationipv4.style.color = "red";
            checkForm = false;
        }
        else {
            startlocationipv4.style.color = "black";
        }
      }
      if (form["geo-ipv4-address"]["end"]!=null) {
          if (!validateIPv4(form["geo-ipv4-address"]["end"])){
              endlocationipv4.style.color = "red";
              checkForm = false;
          }
          else {
              endlocationipv4.style.color = "black";
          }
      }

      if (form["geo-ipv6-address"]["start"]!=null) {
          if (!validateIPv6(form["geo-ipv6-address"]["start"])) {
              startlocationipv6.style.color = "red";
              checkForm = false;
          }
          else {
              startlocationipv6.style.color = "black";
          }
      }
      if (form["geo-ipv6-address"]["end"]!=null) {
          if (!validateIPv6(form["geo-ipv6-address"]["end"])) {
              endlocationipv6.style.color = "red";
              checkForm = false;
          }
          else {
              endlocationipv6.style.color = "black";
          }
      }
      if (form["geo-ipv6-address"]["start"]===null && form["geo-ipv6-address"]["start"]===null ) {
        alert("Either IPv4 or IPv6 address must be filled")
        checkForm = false;
      }

      if (checkForm){
          const response = await fetch('http://115.145.178.185:5000/location/put', {
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
    <div style={{margin:"20px"}}>
        <form onSubmit={handleSubmit}> 
            <div className='nameInput' >
              {/* Name label (first part) of the form */}
              <label htmlFor="country">Country: </label>
              <input name="country" id="country" type="text" required pattern="[a-zA-Z]{2}" placeholder="e.g., 'US', 'JP', and 'PL'" autoComplete='off' value= {form.nameInput}
                onChange={(e) => form['country'] = e.target.value }/> 
            </div>
            
            <br></br>

            <div className='nameInput'>
              {/* Name label (first part) of the form */}
              <label htmlFor="region">Region: </label>
              <input name="region" id="region" type="text" required pattern="[a-zA-Z]{2}-[a-zA-Z0-9]{2,3}" placeholder="e.g., 'ID-RI' and 'NG-RI'" autoComplete='off' value= {form.nameInput}
                onChange={(e) => form['region'] = e.target.value}/> 
            </div>

            <br></br>

            <div className='nameInput'>
              {/* Name label (first part) of the form */}
              <label htmlFor="city">City: </label>
              <input name="city" id="city" type="text" required placeholder="e.g., 'Dublin', 'New York', and 'Sao Paulo'." autoComplete='off' value= {form.nameInput}
                onChange={(e) => form['city'] = e.target.value}/> 
            </div>

            {/* Header for the IP */}
            <h2 style={{textAlign:"center"}}>IP type</h2>

            <div className='ipvs'>
              <tr>
                <td>
                  <label htmlFor="locationipv4">IPv4</label> 
                  <input name='locationipv' id="locationipv4" type="radio" defaultChecked value="ipv4" onClick={() => {setLocationIPv4(true);  
                    setLocationIPv6(false); form['geo-ipv6-address']['start'] = null; form['geo-ipv6-address']['end'] = null}}/>  
                </td>
                <td>
                  <label htmlFor="locationipv6">IPv6</label> 
                  <input name='locationipv' id="locationipv6" type="radio" value="ipv6" onClick={() => {setLocationIPv6(true); 
                    setLocationIPv4(false); 
                    form['geo-ipv4-address']['start'] = null; form['geo-ipv4-address']['end'] = null}}/>
                </td>
              </tr>
                            
              <br></br>

              {/* hidden more ipv4 information (when upper button is clicked) */}
              <div className={locationIPv4 ? "isShown" : "isHidden"}>
                <div id="startlocationipv4">
                  <label htmlFor="locationipv4Start">IPv4-Start: </label>
                  <input type="text" id='locationipv4Start' onChange={(e) => form['geo-ipv4-address']['start'] = e.target.value} autoComplete='off'/>
                </div>
                  <br></br>
                  <br></br>
                <div id="endlocationipv4">
                  <label htmlFor="locationipv4End">IPv4-End: </label>
                  <input type="text" id='deviceipv4End' onChange={(e) => form['geo-ipv4-address']['end'] = e.target.value} autoComplete='off' />
                </div>
              </div>

              {/* hidden more ipv6 information (when upper radio button is clicked) */}

              <div className={locationIPv6 ? "isShown" : "isHidden"}>

                <div id='startlocationipv6'>

                  <label htmlFor="locationipv6Start">IPv6-Start: </label>
                  <input type="text" id='locationipv6Start' autoComplete='off'
                  onChange={(e) => form['geo-ipv6-address']['start'] = e.target.value}/>

                </div>

                <br></br>
                <br></br>

                <div id='endlocationipv6'>
                  <label htmlFor="locationipv6End">IPv6-End: </label>
                  <input type="text" id='locationipv6End' autoComplete='off' onChange={(e) => form['geo-ipv6-address']['end'] = e.target.value}  />
                </div>

              </div>

            </div>

            <br></br>
            <br></br>

            <footer className='footer'>

              <button type='submit' >Submit</button>

            </footer>
        </form>
    </div>
  )
}

export default Locationgroupsform;
