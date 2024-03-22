import React from "react";
import { Link } from "react-router-dom";
import styles from "./NavBar.module.css";
import Logo from "../../assets/logo.png";

const NavBar = ({ data }) => {
  return (
    <div className={styles.box}>
      <Link to="/" className={styles.headerLogo}>
        <img src={Logo} alt="UFCG logo" />
      </Link>
      <nav className={styles.header}>
        <ul className={styles.tabs}>
          {data.map(({name, link}, index) => (
            <li key={index} className={styles.tab}>
              <Link to={link} className={styles.customLink}>
                {name}
              </Link>
            </li>
          ))}
        </ul>
      </nav>
    </div>
  );
};

export default NavBar;