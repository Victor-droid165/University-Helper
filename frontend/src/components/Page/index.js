import React from 'react';
import NavBar from "../NavBar";

const Page = (props) =>  {

    const data = props.data || [
        { name: "Home", link: "/" },
        { name: "Login", link: "/login" },
        { name: "Register", link: "/register" },
        { name: "Help", link: "https://copilot.microsoft.com/" },
    ];

    return (
      <div>
        <NavBar data={data}/> {}
        {props.children} {}
      </div>
    );
}

export default Page;