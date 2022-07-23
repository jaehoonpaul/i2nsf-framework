import Navbar from "./components/navbar";
import Home from "./pages_components/home";
import Configuration from "./pages_components/configuration";
import Registration from "./pages_components/registration";
import './App.css'



function App() {

  let component 
  switch (window.location.pathname) {
    case '/home':
      component = <Home />
      break;
    case '/registration':
      component = < Registration />;
      break;
    case '/configuration':
      component = <Configuration />;
      break;
  }


  return (<> <Navbar /> <div className="container">{component}</div> </>)
}

export default App;
