import React, { useState, useEffect } from 'react';
import './App.css';

function App() {
  const [data, setData] = useState([]);

  useEffect(() => {
    const fetchData = async () => {
      try {
        const response = await fetch('http://localhost:8081/users');
        if (!response.ok) {
          throw new Error('Failed to fetch data');
        }
        const jsonData = await response.json();
        setData(jsonData);
      } catch (error) {
        console.error('Error fetching data:', error);
        setData([]);
    };
    
    };

    fetchData();
  }, []);

  return (
    <div className="App">
      <h1>User List</h1>
      <ul>
        {data.map((user, index) => (
          <li key={index}>
            <strong>Name:</strong> {user.userName}, 
            <strong>Email:</strong> {user.userEmail}, 
            <strong>Matricula::</strong> {user.userEnrollment}, 
            <strong>Senha:</strong> {user.userPassword}, 
            <strong>Universidade</strong> {user.userPassword}
          </li>
        ))}
      </ul>
    </div>
  );
}

export default App;
