import React from "react";
import { Link } from "react-router-dom";
import styles from "./style.module.css";
import Logo from "../../assets/logo.png";

const NavBar = () => {

  return (
    <div className={styles.box}>
      <Link to="/" className={styles.headerLogo}>
        <img src={Logo} alt="UFCG logo" />
      </Link>
      <nav className={styles.header}>
        <ul className={styles.tabs}>
          <li className={styles.tab}>
            <Link to="/" className={styles.customLink}>Home</Link>
          </li>
          <li className={styles.tab}>
            <Link to="/login" className={styles.customLink}>Login</Link>
          </li>
          <li className={styles.tab}>
            <Link to="/register" className={styles.customLink}>Register</Link>
          </li>
          <li className={styles.tab}>
            <a
                target="_blank"
                href="https://chat.openai.com/"
                className={styles.customLink}
            >
                Help
            </a>
          </li>
        </ul>
      </nav>
    </div>
  );
};

export default NavBar;