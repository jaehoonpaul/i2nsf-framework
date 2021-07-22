import React, { Component } from 'react';
import { BrowserRouter, Route,Switch } from 'react-router-dom';
import { Table1,Table2 } from './';

class App extends Component {
    render() {
        return (
            <BrowserRouter>
		<Switch>
                    <Route exact path="/" component={Table1}/>
                    <Route path="/time" component={Table2}/>
		</Switch>
            </BrowserRouter>
        );
    }
}

export default App;
