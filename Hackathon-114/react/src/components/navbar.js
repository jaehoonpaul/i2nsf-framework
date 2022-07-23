export default function Navbar() {
    return <nav className="nav">
        <a href="/home" className="site-title">I2NSF</a>

        <ul className="navbar-items">
            <li>
                <a href="/registration">Registration</a>
            </li>
            <li>
                <a href="/configuration">Configuration</a> 
            </li>
           
        </ul>
    </nav>
}