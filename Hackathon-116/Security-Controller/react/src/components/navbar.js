import React, { useState, useEffect } from "react";
import "./navbar.css"

export default function Navbar() {
    const [navShadow, setnavShadow] = useState("0px 0px 5px");
    const listenScrollEvent = () => {
        window.scrollY > 0 ? setnavShadow("0px 0px 5px") : setnavShadow("0px 0px 5px");
    };
    useEffect(() => {
      window.addEventListener("scroll", listenScrollEvent);
      return () => {
        window.removeEventListener("scroll", listenScrollEvent);
      };
    }, []);
  
    return (
        <nav className="bg-white"
          style={{
            boxShadow: navShadow,
            transition: "all 1s"
          }}
        >
            <a href="/home" className="site-title" style={{fontFamily:"Audiowide"}}>I2NSF</a>

            <ul style={{fontFamily:"Audiowide"}}className="navbar-items">
                <li>
                    <a className="navbar-items" href="/registration">Endpoint</a>
                </li>
                <li>
                    <a className="navbar-items" href="/configuration">Configuration</a> 
                </li>
                <li>
                    <a className="navbar-items" href="/NSFs">NSFs</a> 
                </li>
            </ul>
        </nav>
    );
}