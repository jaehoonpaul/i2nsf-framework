import React from 'react';
import { useState } from 'react';
import './url-group-forms.css'


function Urlgroupsform() {


  // variables for the dropdowns URL 
  const [numberURl, setNumberUrl] = useState([{ urls: '' }])

  const handleUrlAdd = () => {
    setNumberUrl([...numberURl, { urls : ""}])
  }

  const handleUrlRemove = (index) => {
      const list = [...numberURl]
      list.splice(index, 1);
      setNumberUrl(list)
  }

  const handleURLChange = (e,index) => {
    const {name,value} = e.target
    const list = [...numberURl];
    form["urls"][index] = value;
    setNumberUrl(list);
    
  }

  // variables for the registration validation 
  const [form, setForm] = useState({'name':null,'urls':[]});
    
  const handleSubmit = async (e) => {
    e.preventDefault()
    console.log(form)
    let checkForm = true;

    if (checkForm){
      const response = await fetch('http://115.145.178.185:5000/url/put', {
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

      <form>
      
        {/*  Header for the URL GROUPS */}
        <h3 className='urlGroupTitle'>Url Group</h3>

        <div className='nameInput'>

          {/* Name label (first part) of the form */}
        
          <label htmlFor="name">Name: </label>
          <input name="name" id="name" type="text" autoComplete='off'                 
          value= {form.name}
          onChange={(e) => form['name'] = e.target.value}/> 
        
        </div>


        <br></br>
        <br></br>

        
          
          {/* Mac Address label (the second part) the form */}

     


          <div className='urlStuff' >

            <label htmlFor="url">URL: </label>

            {numberURl.map((singleUrlNumber, index) => (

              <div key = {index} className='url' >
                
                <input  name="url" id='url' autoComplete='off' value={numberURl.url} onChange={(e) => {handleURLChange(e,index)}} /> {/*form['url'].push(e.target.value)*/}
                {numberURl.length - 1 === index && numberURl.length < 50 && (
                <button type='button' onClick={handleUrlAdd}><svg id='addIcon' xmlns="http://www.w3.org/2000/svg" width="192" height="192" fill="#000000" viewBox="0 0 256 256"><rect width="256" height="256" fill="none"></rect><line x1="40" y1="128" x2="216" y2="128" fill="none" stroke="#000000" stroke-linecap="round" stroke-linejoin="round" stroke-width="16"></line><line x1="128" y1="40" x2="128" y2="216" fill="none" stroke="#000000" stroke-linecap="round" stroke-linejoin="round" stroke-width="16"></line></svg></button>
                )}
                
                {numberURl.length > 1 && 
                (<button type='button' onClick={() => handleUrlRemove(index)}><svg id='subIcon' xmlns="http://www.w3.org/2000/svg" width="192" height="192" fill="#000000" viewBox="0 0 256 256"><rect width="256" height="256" fill="none"></rect><line x1="40" y1="128" x2="216" y2="128" fill="none" stroke="#000000" stroke-linecap="round" stroke-linejoin="round" stroke-width="16"></line></svg></button>
                )}
                
              </div>
            ))}
            

          
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

export default Urlgroupsform;