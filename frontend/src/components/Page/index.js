import React from 'react';
import NavBar from "../NavBar";
import { Container } from '@mui/material';
import styles from './style.module.css'

const Page = (props) =>  {
    const data = props.data || [
      { name: "Home", link: "/" },
      { name: "Login", link: "/login" },
      { name: "Register", link: "/register" },
      { name: "Help", link: "https://copilot.microsoft.com/" },
    ];

    return (
      <div>
        <NavBar data={data}/>
        <div className={styles.mainContent}>
          {props.children}
        </div>
      </div>
    );
}

export default Page;