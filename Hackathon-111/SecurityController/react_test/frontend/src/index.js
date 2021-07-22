import React from 'react';
import ReactDOM from 'react-dom';
import './index.css';
import App from './App';
import reportWebVitals from './reportWebVitals';
export { default as RealTimeChart} from './RealTimeChart';
export { default as RealTimeChart2} from './RealTimeChart2';
export { default as Table1} from './Table1';
export { default as Table2} from './Table2';
export { default as RealTimeChart3} from './RealTimeChart3';
export { default as RealTimeChart5} from './RealTimeChart5';
export { default as RealTimeChart6} from './RealTimeChart6';
ReactDOM.render(
  <React.StrictMode>
    <App />
  </React.StrictMode>,
  document.getElementById('root')
);

// If you want to start measuring performance in your app, pass a function
// to log results (for example: reportWebVitals(console.log))
// or send to an analytics endpoint. Learn more: https://bit.ly/CRA-vitals
reportWebVitals();
