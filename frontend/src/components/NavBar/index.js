import React from "react";
import { Link } from "react-router-dom";
import styles from "./style.module.css";
import Logo from "../../assets/logo.png";

const NavBar = ({ data }) => {
  return (
    <div className={styles.box}>
      <Link to="/" className={styles.headerLogo}>
        <img src={Logo} alt="UFCG logo" />
      </Link>
      <nav className={styles.header}>
        <ul className={styles.tabs}>
          {data.map((item, index) => (
            <li key={index} className={styles.tab}>
              <Link to={item.link} className={styles.customLink}>
                {item.name}
              </Link>
            </li>
          ))}
        </ul>
      </nav>
    </div>
  );
};

export default NavBar;