import logo from './logo.svg';
import './App.css';
import { useHistory } from 'react-router-dom';
import {RealTimeChart, RealTimeChart3} from '.';


const Table1 = ()=> {

	const history = useHistory();
	const componentClicked = (e) =>{
	if (e==='time'){
	history.push({pathname:"/time"})
	}
	}


  return (
    <div className="App">
      <header style={{backgroundColor: "gray"}} className="App-header">
        <div style={{fontWeight:'600', fontSize:'40px'}}>
<p>
          URL Filtering Resources Data
</p>

</div>
<button onClick={(e)=> componentClicked('time')}>Firewall</button>
<div style={{display:'flex'}}>
<div style={{fontSize:'20px', marginTop:'20px', marginRight:'20px', width:'40%', textAlign:'left'}}>
NSF Name : url_filtering
<br></br>
Platform : Linux64
<br></br>
OS : Ubuntu 16.04.7 LTS
<br></br>
Total RAM : 2 GB
<br></br>
CPU Model Name : QEMU Virtual CPU version 2.5+ @ 2.4 GHz
<br></br>
Disk size : 20 GB
</div>
<div>
        <RealTimeChart>
	</RealTimeChart>
	<RealTimeChart3>
	</RealTimeChart3>      
</div>
</div>	
</header>
</div>
  );
}

export default Table1;
