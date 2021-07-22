import logo from './logo.svg';
import './App.css';
import {RealTimeChart, RealTimeChart2} from '.';

const App = ()=> {


  return (
    <div className="App">
      <header style={{backgroundColor: "gray"}} className="App-header">
        <div style={{fontWeight:'600', fontSize:'40px'}}>
<p>
          URL Filtering Resources Data
</p>
</div>
        <RealTimeChart>
	</RealTimeChart>	
       
	</header>
    </div>
 
  );
}

export default App;
