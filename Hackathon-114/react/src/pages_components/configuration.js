import React from 'react';
import './configuration.css'
import Resultmodal from '../modals_components/resultmodal';
import { useState } from 'react'



export default function Configuration() {

  const [openModal, setOpenModal] = useState(false);

const [PolicyName, setPolicyName]= useState("");
const [PolicyLanguage, setPolicyLanguage] = useState("");
const [ResolutionStrategy, setResolutionStrategy]= useState("")


// Rule
const [RuleName, setRuleName] = useState("");
const [PriorityName, setPriorityName] = useState("");
const [SystemEvent, setSystemEvent] = useState("");
const [Systemevent, setSystemAlarm] = useState("");

//Condition
const [FirewallSource, setFirewallSource] = useState("");
const [FirewallDest, setFirewallDest] = useState("");
const [FirewallSystemAlarm, setFirewallSystemAlarm] = useState("");
const [FirewallStartPortNum, setFirewallStartPortNum] = useState("");
const [FirewallEndPortNum, setFirewallEndPortNum] = useState("");
const [FirewallIcmpMessage, setFirewallIcmpmessage] = useState("");

// DDOS
const [DdosPacketRateThreshod, setDdosPacketRateThreshod] = useState("");
const [DdosByteRateThreshod, setDdosByteRateThreshod] = useState("");
const [DdosFlowRateThreshod, setDdosFlowRateThreshod] = useState("");
 
// Antivirus
const [ExceptionFiles, setExceptionFiles] =useState("");

// Payload
const [PayloadContent,setPayloadContent] = useState("")

// URL-Categroy
const [UrlName,setUrlName] = useState("")

// Voice
const [VoiceSourceId,setVoiceSourceId] = useState("")
const [VoiceDestId,setVoiceDestId] = useState("")
const [VoiceUserAgent,setVoiceUserAgent] = useState("")

// Context Time
const [ContextStartDateTime,setContextStartDateTime] = useState("")
const [ContextEndDateTime,setContextEndDateTime] = useState("")



// variables for the frequency buttons 
// only once
const [frequencyOnlyOnce, setFrequencyOnlyOnce] = useState(false);

const [StartTime, setStartTime] = useState("");
const [EndTime, setEndTime] = useState("");


// weekly
const [frequencyWeekly, setFrequencyWeekly] = useState(false); 
const [Day, setDay] = useState("");

// monthly
const [frequencyMonthly, setFrequencyMonthly] = useState(false);
const [MonthlyDay, setMonthlyDay] = useState("");

// yearly
const [frequencyYearly, setFrequencysYearly] = useState(false);   
const [YearlyMonth, setYearlyMonth] = useState("");

// Application
const [ApplicationProtocal,setApplicationProtocal] = useState("")

// Device Type
const [DeviceType,setDeviceType] = useState("")

//User
const [UserID, setUserID] = useState();
const [UserName, setUserName] = useState();

// Group
const [GroupID, setGroupID] = useState();
const [GroupName, setGroupName] = useState();


// variables for the users radio buttons 
const [oneUser, setOneUser] = useState(false);
const [groupUser, setGroupUser] = useState(false);

// Geograpghic Location
const [GeoSource, setGeoSource] = useState("");
const [GeoDest, setGeoDest] = useState("");

// Thread-Feed
const [ThreadName, setThreadName] = useState("");

// Action
const [PrimaryAction, setPrimaryAction ] = useState("");
const [SeciondaryAction, setSecondaryAction ] = useState("");


const [Policy_infos_form, setPolicyInfosform] = useState({
  "i2nsf-cfi-policy": { 
      "name": null,
      "language": null,
      "resolution-strategy":null,
      "rules" : {
          "name":null,
          "priority":null,
          "event":{
              "system-event":null,
              "system-alarm":null
          },
          "condition": {
              "firewall": {
                  "source":null,
                  "destination":null,
                  "transport-layer-protocol": null,
                  "range-port-number": {
                      "start-port-number":null,
                      "end-port-number":null
                  },
                  "icmp": {
                      "icmp-message":null
                  }
              },
              "ddos" : {
                  "rate-limit" : {
                      "packet-rate-threshold":null,
                      "byte-rate-threshold":null,
                      "flow-rate-threshold":null
                  }
              },
              "anti-virus" : {
                  "exception-file":null
              },
              "payload" : {
                  "content" : null
              },
              "url-category": {
                  "url-name" :null
              },                
              "voice" : {
                  "source-id": null,
                  "destination-id":null,
                  "user-agent":null
              },
              "context": {
                  "time": {
                      
                      "start-date-time":null,
                      "end-date-time":null,
                      "period": {
                          "start-time":null,
                          "end-time" :null,
                          "day":null,
                          "date":null,
                          "month":null
                      },
                      "frequency" : null
                  },
                  "application": {
                      "protocol": null
                  },
                  "device-type": {
                      "device": null
                  },
                  "users": {
                      "user":{
                          "id":null,
                          "name":null
                      },
                      "group":{
                          "id":null,
                          "name":null
                      }
                  },
                  "geographic-location":{
                      "sorce":null,
                      "destination":null
                  }
              },
              "thread-feed": {
                  "name":null
              }
          },

          "action":{
              "primary-action": {
                  "action": null
              },
              "secondary-action": {
                  "log-action": null
              }
          }
      }
  }
});
var temp2= Policy_infos_form

const [xml, setxml ] = useState("");


const ClickSubmit =async (e) => {
  e.preventDefault()

  if(frequencyOnlyOnce ===true){
    temp2['i2nsf-cfi-policy'].rules.condition.context.time.frequency = "only-once"
    setPolicyInfosform(temp2)
  } else if( frequencyWeekly ===true){
    temp2['i2nsf-cfi-policy'].rules.condition.context.time.frequency = "weekly"
    setPolicyInfosform(temp2)

  } else if( frequencyMonthly ==true){
    temp2['i2nsf-cfi-policy'].rules.condition.context.time.frequency = "monthly"
    setPolicyInfosform(temp2)

  } else if( frequencyYearly ==true){
    temp2['i2nsf-cfi-policy'].rules.condition.context.time.frequency = "yearly"
    setPolicyInfosform(temp2)}


  const response = await fetch('http://115.145.178.185:5000/high_level', {
      method: 'PUT',
      body: JSON.stringify(Policy_infos_form),
      headers: {
          'Content-Type': 'application/json'
      }
  })
              
  const myJson = await response.json();
  alert(myJson)
  setxml(myJson)


}


const handleChange =(e) => {
  var temp = Policy_infos_form

  // Policy ok
  if(e.target.name==="name"){
    temp['i2nsf-cfi-policy'].name = e.target.value
    setPolicyInfosform(temp)
    console.log(Policy_infos_form)
    setPolicyName(e.target.value)
  }
  else if(e.target.name==="language"){
    setPolicyLanguage(e.target.value)
    temp['i2nsf-cfi-policy'].language = e.target.value
    console.log(Policy_infos_form)
    setPolicyInfosform(temp)
  }
  else if(e.target.name==="resolutionStrategy"){
    temp['i2nsf-cfi-policy']['resolution-strategy'] = e.target.value
    setPolicyInfosform(temp)
    console.log(Policy_infos_form)
    setPolicyLanguage(e.target.value)
  }
  // Rule ok
  else if(e.target.name==="rulename"){
    temp['i2nsf-cfi-policy'].rules.name = e.target.value
    setPolicyInfosform(temp)
    console.log(Policy_infos_form)
    setRuleName(e.target.value)
  }
  else if(e.target.name==="priority"){
    temp['i2nsf-cfi-policy'].rules.priority = e.target.value
    setPolicyInfosform(temp)
    console.log(Policy_infos_form)
    setPriorityName(e.target.value)
  }
  else if(e.target.name==="configEvent"){
    temp['i2nsf-cfi-policy'].rules.event['system-event']= e.target.value
    setPolicyInfosform(temp)
    console.log(Policy_infos_form)
    setSystemEvent(e.target.value)
  }
  else if(e.target.name==="configAlarm"){
    temp['i2nsf-cfi-policy'].rules.event['system-alarm'] = e.target.value
    setPolicyInfosform(temp)
    console.log(Policy_infos_form)
    setSystemAlarm(e.target.value)
  }
  // Condition ok
  else if(e.target.name==="firewallsource"){
    temp['i2nsf-cfi-policy'].rules.condition.firewall.source = e.target.value
    setPolicyInfosform(temp)
    setFirewallSource(e.target.value)
  }
  else if(e.target.name==="firewalldestination"){
    temp['i2nsf-cfi-policy'].rules.condition.firewall.destination = e.target.value
    setPolicyInfosform(temp)
    setFirewallSource(e.target.value)
  }
  else if(e.target.name==="firewallconfigTransportProtocol"){
    temp['i2nsf-cfi-policy'].rules.condition.firewall['transport-layer-protocol']= e.target.value
    setPolicyInfosform(temp)
    setFirewallSystemAlarm(e.target.value)
  }
  else if(e.target.name==="start-port-number"){
    temp['i2nsf-cfi-policy'].rules.condition.firewall['range-port-number']['start-port-number'] = e.target.value
    setPolicyInfosform(temp)
    setFirewallStartPortNum(e.target.value)
  }
  else if(e.target.name==="end-port-number"){
    temp['i2nsf-cfi-policy'].rules.condition.firewall['range-port-number']['end-port-number']= e.target.value
    setPolicyInfosform(temp)
    setFirewallEndPortNum(e.target.value)
  }
  else if(e.target.name==="icmpMessage"){
    temp['i2nsf-cfi-policy'].rules.condition.firewall.icmp['icmp-message'] = e.target.value
    setPolicyInfosform(temp)
    setFirewallIcmpmessage(e.target.value)
    console.log(temp)
  }

 // DDos  ok
  else if(e.target.name==="packet-rate-threshold"){
    temp['i2nsf-cfi-policy'].rules.condition.ddos['rate-limit']['packet-rate-threshold']= e.target.value
    setPolicyInfosform(temp)
    setFirewallIcmpmessage(e.target.value)
  }
  else if(e.target.name==="byte-rate-threshold"){
    temp['i2nsf-cfi-policy'].rules.condition.ddos['rate-limit']['byte-rate-threshold'] = e.target.value
    setPolicyInfosform(temp)
    setFirewallIcmpmessage(e.target.value)
  }
  else if(e.target.name==="flow-rate-threshold"){
    temp['i2nsf-cfi-policy'].rules.condition.ddos['rate-limit']['flow-rate-threshold'] = e.target.value
    setPolicyInfosform(temp)
    setFirewallIcmpmessage(e.target.value)
  }

  // Anti virus
  else if(e.target.name==="exception-files"){
    temp['i2nsf-cfi-policy'].rules.condition['anti-virus'] = e.target.value
    setPolicyInfosform(temp)
    setFirewallIcmpmessage(e.target.value)
  }

  // Payload
  else if(e.target.name==="content"){
    temp['i2nsf-cfi-policy'].rules.condition.payload.content = e.target.value
    setPolicyInfosform(temp)
    setPayloadContent(e.target.value)
  }
  else if(e.target.name==="url-category"){
    temp['i2nsf-cfi-policy'].rules.condition['url-category']['url-name']= e.target.value
    setPolicyInfosform(temp)
    setUrlName(e.target.value)
  }
  // Voice
  else if(e.target.name==="source-id"){
    temp['i2nsf-cfi-policy'].rules.condition.voice['source-id']= e.target.value
    setPolicyInfosform(temp)
    setVoiceSourceId(e.target.value)
  }
  else if(e.target.name==="destination-id"){
    temp['i2nsf-cfi-policy'].rules.condition.voice['destination-id']= e.target.value
    setPolicyInfosform(temp)
    setVoiceDestId(e.target.value)
  }
  else if(e.target.name==="user-agent"){
    temp['i2nsf-cfi-policy'].rules.condition.voice['user-agent'] = e.target.value
    setPolicyInfosform(temp)
    setVoiceUserAgent(e.target.value)
  }

  // context time
  else if(e.target.name==="start-date-time"){
    temp['i2nsf-cfi-policy'].rules.condition.context.time['start-date-time']= e.target.value
    setPolicyInfosform(temp)
    setContextStartDateTime(e.target.value)
  }
  else if(e.target.name==="end-date-time'"){
    temp['i2nsf-cfi-policy'].rules.condition.context.time['end-date-time']= e.target.value
    setPolicyInfosform(temp)
    setContextEndDateTime(e.target.value)
  }

  //Period
  else if(e.target.name==="start-time"){
    temp['i2nsf-cfi-policy'].rules.condition.context.time.period['start-time']= e.target.value
    setPolicyInfosform(temp)
    setContextEndDateTime(e.target.value)
  }
  else if(e.target.name==="end-time"){
    temp['i2nsf-cfi-policy'].rules.condition.context.time.period['end-time']= e.target.value
    setPolicyInfosform(temp)
    setContextEndDateTime(e.target.value)
  }// Weekly
  else if(e.target.name==="day"){
    temp['i2nsf-cfi-policy'].rules.condition.context.time.period.day = e.target.value
    setPolicyInfosform(temp)
    setDay(e.target.value)
  }// Monthly
  else if(e.target.name==="Monthlydate"){
    temp['i2nsf-cfi-policy'].rules.condition.context.time.period.date = e.target.value
    setPolicyInfosform(temp)
    setMonthlyDay(e.target.value)
  }// Yearly
  else if(e.target.name==="YearlyMonth"){
    temp['i2nsf-cfi-policy'].rules.condition.context.time.period.month= e.target.value
    setPolicyInfosform(temp)
    setYearlyMonth(e.target.value)  
  }
  
  // Frequency
  else if(e.target.name==="YearlyMonth"){
    temp['i2nsf-cfi-policy'].rules.condition.context.time.period.month= e.target.value
    setPolicyInfosform(temp)
    setYearlyMonth(e.target.value)  
  }
  
  // Application 
  else if(e.target.name==="applicationProtocol"){
    temp['i2nsf-cfi-policy'].rules.condition.context.application.protocol = e.target.value
    setPolicyInfosform(temp)
    setApplicationProtocal(e.target.value)
  } 
  else if(e.target.name==="deviceType"){
    temp['i2nsf-cfi-policy'].rules.condition.context['device-type'].device = e.target.value
    setPolicyInfosform(temp)
    setDeviceType(e.target.value)
  } 
  // Users
  else if(e.target.name==="oneUserID"){
    temp['i2nsf-cfi-policy'].rules.condition.context.users.user.id= e.target.value
    setPolicyInfosform(temp)
    setUserID(e.target.value)
  } 
  else if(e.target.name==="oneUserName"){
    temp['i2nsf-cfi-policy'].rules.condition.context.users.user.name = e.target.value
    setPolicyInfosform(temp)
    setUserName(e.target.value)
  } 
  // Group
  else if(e.target.name==="groupUserID"){
    temp['i2nsf-cfi-policy'].rules.condition.context.users.group.id = e.target.value
    setPolicyInfosform(temp)
    setGroupID(e.target.value)
  } 
  else if(e.target.name==="groupUserName"){
    temp['i2nsf-cfi-policy'].rules.condition.context.users.group.name = e.target.value
    setPolicyInfosform(temp)
    setGroupName(e.target.value)
  } 

  // Geo
  else if(e.target.name==="GeoSource"){
    temp['i2nsf-cfi-policy'].rules.condition.context['geographic-location'].sorce= e.target.value
    setPolicyInfosform(temp)
    setGeoSource(e.target.value)
  } 
  else if(e.target.name==="GeoDestination"){
    temp['i2nsf-cfi-policy'].rules.condition.context['geographic-location'].destination = e.target.value
    setPolicyInfosform(temp)
    setGeoDest(e.target.value)
  } 

  // Thread-feed
  else if(e.target.name==="threatfeed"){
    temp['i2nsf-cfi-policy'].rules.condition['thread-feed'].name = e.target.value
    setPolicyInfosform(temp)
    setThreadName(e.target.value)
  } 

  // ACtion
  else if(e.target.name==="primaryAction"){
    temp['i2nsf-cfi-policy'].rules.action["primary-action"]['action'] = e.target.value
    setPolicyInfosform(temp)
    setPrimaryAction(e.target.value)
    console.log(temp)
  } 
  else if(e.target.name==="secondaryAction"){
    temp['i2nsf-cfi-policy'].rules.action['secondary-action']["log-action"] = e.target.value
    setPolicyInfosform(temp)
    setSecondaryAction(e.target.value)
    console.log(temp)
  } 


}

  return (
    <div className='configuration'>
        <h1>Configuration</h1>


        <form>
        
        <fieldset>

        <h1>i2nsf-cfi-policy</h1>

        <fieldset>

          {/* name */}
          <div className='configName'>


            <label htmlFor='name'>Name:</label>

            <input 
              id='name'
              name='name'
              type="text" 
              onChange={handleChange}
            />

          </div>

          <br></br>
          <br></br>

          {/* language */}
          <div className='configLanguage'>

            <label htmlFor="language">Language: </label>
            <input id='language'
              name='language'
              type="text"
              onChange={handleChange}
             />


          </div>

          <br></br>
          <br></br>

          {/* resolution strategy */}
          <div className='configResolutionStrategy'>
              <label htmlFor='resolutionStrategy'>Resolution Strategy: </label>
              <select name="resolutionStrategy" id="continent" onChange={handleChange}>
              <option value=""></option>
              <option value="fmr">fmr</option>
              <option value="lmr">lmr</option>
              <option value="pmre">pmre</option>
              <option value="pmrn">pmrn</option>
              </select>
          </div>

          </fieldset>


          <br></br>

          {/* rules */} 

          <fieldset>

            <div className='configRules'>

              <h3>RULES</h3>

                <div className='configRulesName'>
                
                <label htmlFor="rulename">Name: </label>
                <input id="rulename" name="rulename" type="text" onChange={handleChange}/>

              </div>

              <br></br>
              <br></br>


              <div className='configRulePriority'>

                {/* rule priority which later needs regex added */}


                <label htmlFor="rulepriority">Priority: </label>
                <input id="rulepriority" name="priority" type="text" onChange={handleChange}/>

              </div>

              <br></br>
              <br></br>

              <div className='configEvent'>
                <label htmlFor='configEvent'>System Event: </label>

                <select name="configEvent" id="configEvent" onChange={handleChange}>
                  <option value=""></option>
                  <option value="access-violation">access-violation</option>
                  <option value="configuration-change">configuration-change</option>
                </select>

              </div>


              <br></br>
              <br></br>

              <div className='configAlarm'>
                <label htmlFor='configAlarm'>System Alarm: </label>

                <select name="configAlarm" id="configAlarm" onChange={handleChange}>
                  <option value=""></option>
                  <option value="memory-alarm">memory-alarm</option>
                  <option value="cpu-alarm">cpu-alarm</option>
                  <option value="disk-alarm">disk-alarm</option>
                  <option value="hardware-alarm">hardware-alarm</option>
                  <option value="interface-alarm">interface-alarm</option>
                </select>
                
              </div>

              <br></br>
              
              <div className='configCondition'>
                <p><b>CONDITION</b></p>
                <p>FIREWALL</p>

                <label htmlFor="source">Source: </label>
                <input id='source' name='firewallsource' type='union' onChange={handleChange}/>

                <br></br>
                <br></br>


                <label htmlFor="destination">Destination: </label>
                <input id='destination' name='firewalldestination' type='union' onChange={handleChange}/>


                <br></br>
                <br></br>

                <div className='transportProtocol'>
                  <label htmlFor='configTransportProtocol'>System Alarm: </label>

                  <select name="firewallconfigTransportProtocol" id="configTransportProtocol" onChange={handleChange}>
                    <option value=""></option>
                    <option value="tcp">tcp</option>
                    <option value="udp">udp</option>
                    <option value="sctp">sctp</option>
                    <option value="dccp">dccp</option>
                  </select>

                </div>


              </div>

              <br></br>

              <div className='portNumber'>

                

                <div className='rangePortNumber'>
                  <label htmlFor="start-port-number">Start-port-number: </label>
                  <input id="start-port-number" name='start-port-number' type='text' onChange={handleChange}/>

                  <br></br>
                  <br></br>

                  <label htmlFor="end-port-number">End-port-number: </label>
                  <input id="end-port-number" name='end-port-number' type='text' onChange={handleChange}/>
                </div>

              </div>

              <br></br>
              <br></br>

              {/* icmp message */}

              <div className='icmp'>
                  <label htmlFor='icmpMessage'>Icmp Message: </label>

                  <select name="icmpMessage" id="icmpMessage" onChange={handleChange}>
                    <option value=""></option>
                    <option value="echo reply">echo reply</option>
                    <option value="destination-unreachable">destination-unreachable</option>
                    <option value="redirect">redirect</option>
                    <option value="echo">echo</option>
                    <option value="router-advertisement">router-advertisement</option>
                    <option value="router-solicitation">router-solicitation</option>
                    <option value="time-exceeded">time-exceeded</option>
                    <option value="parameter-problem">parameter-problem</option>
                    <option value="experimental-mobility-protocols">experimental-mobility-protocols</option>
                    <option value="extended-echo-request">extended-echo-request</option>
                    <option value="extended-echo-reply">extended-echo-reply</option>
                  </select>

              </div>


              <br></br>
              <br></br>


              {/* ddos section */}
              <div className='ddos'>

                <p>DDOS</p>

                <label htmlFor="packet-rate-threshold">Packet-rate-threshold: </label>
                <input id='packet-rate-threshold' name='packet-rate-threshold' type='text' onChange={handleChange}/>

                <br></br>
                <br></br>

                <label htmlFor="byte-rate-threshold">Byte-rate-threshold: </label>
                <input id='byte-rate-threshold' name='byte-rate-threshold' type='text' onChange={handleChange}/>

                <br></br>
                <br></br>

                <label htmlFor="flow-rate-threshold">Flow-rate-threshold: </label>
                <input id='flow-rate-threshold' name='flow-rate-threshold' type='text' onChange={handleChange}/> 

             </div>

             <br></br>
             <br></br>

             {/* This is the antivirus section  */}

             <div className='antivirus'>

              <p>ANTI-VIRUS</p>

              <label htmlFor="exception-files">Exception-files: </label>
              <input id='exception-files' name='exception-files' type='text' onChange={handleChange}/>

             </div>

             <br></br>
             <br></br>

             {/* This is the payload section */}


             <div className='payload'>

              <p>PAYLOAD</p>

              <label htmlFor="content">Content: </label>
              <input id='content' name='content' type='text' onChange={handleChange} />

            </div>


            <br></br>
            <br></br>

             {/* This is the URl category section */}


             <div className='url-category'>

              <p>URL-CATEGORY</p>

              <label htmlhtmlFor="url-category">Url-name: </label>
              <input id='url-category' name='url-category' type='text' onChange={handleChange}/>

            </div>

            <br></br>
            <br></br>

             {/* Voice section */}


             <div className='voice'>

              <p>VOICE</p>

              <label htmlFor="source-id">Source-id: </label>
              <input id='source-id' name='source-id' type='text' onChange={handleChange}/>

              <br></br>
              <br></br>

              <label htmlFor="destination-id">Destination-id: </label>
              <input id='destination-id' name='destination-id' type='text' onChange={handleChange} />

              
              <br></br>
              <br></br>

              <label htmlFor="user-agent">User-agent: </label>
              <input id='user-agent' name='user-agent' type='text' onChange={handleChange}/>

            </div>
            

          
            <br></br>
            <br></br>

             {/* Context */}


             <div className='context'>

              <p>CONTEXT TIME </p>

              <label htmlFor="start-date-time">Start-date-time: </label>
              <input id='start-date-time' name='start-date-time' type='text' onChange={handleChange} />

              <br></br>
              <br></br>

              <label htmlFor="end-date-time">End-date-time: </label>
              <input id='end-date-time' name='end-date-time' type='text' onChange={handleChange} />

              
              <br></br>
              <br></br>

                <p>PERIOD</p>


                <label>Frequency: </label>

                <br></br>
                <br></br>

                <label htmlFor="once" className='frequencyLabel'>Only Once: </label> 
                <input name='frequency' id="once" type="radio" value="only-once" className='frequencyRadio'  onClick={() => {setFrequencyOnlyOnce(true); setFrequencyWeekly(false) ;setFrequencyMonthly(false); setFrequencysYearly(false);}}/>

                <label htmlFor="weekly" className='frequencyLabel'>Weekly: </label> 
                <input name='frequency' id="weekly" type="radio" value="weekly" className='frequencyRadio' onClick={() => {setFrequencyOnlyOnce(false); setFrequencyWeekly(true) ;setFrequencyMonthly(false); setFrequencysYearly(false);}}/>

                <label htmlFor="monthly" className='frequencyLabel'>Monthly: </label> 
                <input name='frequency' id="monthly" type="radio" value="monthly" className='frequencyRadio' onClick={() => {setFrequencyOnlyOnce(false); setFrequencyWeekly(false) ;setFrequencyMonthly(true); setFrequencysYearly(false);}}/>


                <label htmlFor="yearly" className='frequencyLabel'>Yearly: </label> 
                <input name='frequency' id="yearly" type="radio" value="yearly" className='frequencyRadio' onClick={() => {setFrequencyOnlyOnce(false); setFrequencyWeekly(false) ;setFrequencyMonthly(false); setFrequencysYearly(true);}}  />



                <br></br>
                <br></br>
                <br></br>
                <br></br>
                <br></br>
                <br></br>
                


                <div className={frequencyOnlyOnce ? "isShown" : "isHidden"}>
                  <label htmlFor='start-time'>Start-time: </label>
                  <input id='start-time' name='start-time' type='time' onChange={handleChange}/>

                  <br></br>
                  <br></br>

                  <label htmlFor='end-time'>End-time: </label>
                  <input id='end-time' name='end-time' type='time' onChange={handleChange}/>
                </div>

                <br></br>
                <br></br>

                <div className={frequencyWeekly ? "isShown" : "isHidden"}>
                  <label htmlFor='day'>Day: </label>
                  <input id='day' name='day' type='day' onChange={handleChange}/>
                </div>


                <br></br>
                <br></br>

                <div className={frequencyMonthly ? "isShown" : "isHidden"}>
                  <label htmlFor='date'>Date: </label>
                  <input id='date' name='Monthlydate' type='date' onChange={handleChange}/>
                </div>


                <br></br>
                <br></br>

                <div className={frequencyYearly ? "isShown" : "isHidden"}>
                  <label htmlFor='month'>Month: </label>
                  <input id='month' name='YearlyMonth' type='month' onChange={handleChange}/>
                </div>


                <br></br>
                <br></br>
                <br></br>
                

                <p>APPLICATION</p>

                <label htmlFor='applicationProtocol'>Application Protocol: </label>
                <select id="applicationProtocol" name='applicationProtocol' onChange={handleChange} >
                    <option value=""></option>
                    <option value="http">http</option>
                    <option value="https">https</option>
                    <option value="http2">http2</option>
                    <option value="https2">https2</option>
                    <option value="ftp">ftp</option>
                    <option value="ssh">ssh</option>
                    <option value="telnet">telnet</option>
                    <option value="smtp">smtp</option>
                    <option value="pop3">pop3</option>
                    <option value="pop3s">pop3s</option>
                    <option value="imap">imap</option>
                    <option value="imaps">imaps</option>
                </select>

              
              <p>DEVICE TYPE</p>
              <label htmlFor='deviceType'>Device Type: </label>
              <select id='deviceType' name='deviceType' onChange={handleChange}>
                  <option value=''></option>
                  <option value='computer'>computer</option>
                  <option value='mobile-phone'>mobile-phone</option>
                  <option value='voip-vocn-phone'>voip-vocn-phone</option>
                  <option value='tablet'>tablet</option>
                  <option value='network-infrastructure-device'>network-infrastructure-device</option>
                  <option value='iot-device'>iot-device</option>
                  <option value='ot'>ot</option>
                  <option value='vehicle'>vehicle</option>
              </select>

              <br></br>
              <br></br>
              <br></br>
              <br></br>

              <p>USERS</p>

              <label htmlFor="oneUser" className='userLabel'>User</label> 
              <input name='users' id="oneUser" type="radio" value="user" className='userRadio'  onClick={() => {setOneUser(true); setGroupUser(false)}}/>

              <label htmlFor="groupUser" className='userLabel'>Group</label> 
              <input name='users' id="groupUser" type="radio" value="group" className='userRadio' onClick={() =>{setOneUser(false); setGroupUser(true)}} />

              
              <br></br>
              <br></br>
              <br></br>
              <br></br>

              {/* hidden section that appear after one of the radio buttons has been clicked */}


              {/* One user only */}

              <div className={oneUser ? "isShown" : "isHidden"}>
                
                <label htmlFor='oneUser'>ID: </label>
                <input id='oneUser' name='oneUserID' onChange={handleChange} />

                <br></br>
                <br></br>

                <label>Name: </label>
                <input id='oneUser' name='oneUserName' onChange={handleChange}/>

              </div>

              {/* Group user */}


              <div className={groupUser ? "isShown" : "isHidden"}>
                
                <label htmlFor='groupUser'>ID: </label>
                <input id='groupUser' name='groupUserID' onChange={handleChange}/>

                <br></br>
                <br></br>

                <label>Name: </label>
                <input id='groupUser' name='groupUserName' onChange={handleChange}/>

              </div>   

              <br></br>
              <br></br>
              <br></br>
              <br></br>

              {/* geographic location */}

              <p>GEOGRAPHIC LOCATION</p>

              <label htmlFor='source'>Source: </label>
              <input id='source' name='GeoSource' onChange={handleChange} />

              <br></br>
              <br></br>

              <label htmlFor='destination'>Destination: </label>
              <input id='destination' name='GeoDestination' onChange={handleChange} />



              </div>

              <br></br>
              <br></br>
              <br></br>
              <br></br>

              <p>THREAD-FEED</p>

              <label htmlFor='threatfeed'>Name: </label>
              <input id='threatfeed' name='threatfeed' onChange={handleChange}/>

            </div>

            <br></br>
            <br></br>
            <br></br>
            <br></br>


            <div className='configAction'>


              <h3><b>ACTION</b></h3>

              <label htmlFor='primaryAction'>Primary Action</label>
              <select id='primaryAction' name='primaryAction'onChange={handleChange}>

                <option value = ''></option>
                <option value='pass'>pass</option>
                <option value='drop'>drop</option>
                <option value='reject'>reject</option>
                <option value='rate-limit'>rate-imit</option>
                
              </select>

              <br></br>
              <br></br>

              <label htmlFor='secondaryAction'>Secondary Action</label>

              <select id='secondaryAction' name='secondaryAction' onChange={handleChange}>

                <option value = ''></option>
                <option value='rule log'>rule log</option>
                <option value='session log'>session log</option>                

              </select>

            </div>

          </fieldset>





        </fieldset>

      </form>
        <button className='openModalBtn' onClick={(e) => {setOpenModal(true);ClickSubmit(e)}}>Submit</button>
        {openModal && <Resultmodal closeModal={setOpenModal} data={xml}/>} 
    </div>  
  )
}