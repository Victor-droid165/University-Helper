import React from 'react';
import NavBar from "../NavBar";

const Page = (props) =>  {
    return (
      <div>
        <NavBar /> {}
        {props.children} {}
      </div>
    );
}

export default Page;