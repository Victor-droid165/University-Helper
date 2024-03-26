import React from 'react';
import './App.css';

import router from './Routes/Routes.js';
import { RouterProvider } from 'react-router-dom';
import { AuthProvider } from './contexts/Auth/AuthProvider.js';

function App() {

  // Criação de usuário 

  // const user = {
  //   userType: "student",
  //   userName: "João",
  //   userUniversity: "ufcg",
  //   userEnrollment: "1202210000",
  //   userEmail: "joao@gmail.com",
  //   userPassword: "12345678"
  // };

  // fetch('http://localhost:8081/register', {
  //   method: 'Post',
  //   headers: {
  //     'Content-Type': 'application/json',
  //   },
  //   body: JSON.stringify(user),
  // })
  // .then(response => response.json())
  // .then(data => {
  //   console.log('Success:', data);
  // })
  // .catch((error) => {
  //   console.error('Error:', error);
  // });

  return (
    <AuthProvider>
      <RouterProvider router={router} />
    </AuthProvider>
  );

  // const [data, setData] = useState([]);

  // useEffect(() => {
  //   const fetchData = async () => {
  //     try {
  //       const response = await fetch('http://localhost:8081/users');
  //       if (!response.ok) {
  //         throw new Error('Failed to fetch data');
  //       }
  //       const jsonData = await response.json();
  //       setData(jsonData);
  //     } catch (error) {
  //       console.error('Error fetching data:', error);
  //       setData([]);
  //   };

  //   };

  //   fetchData();
  // }, []);

  // return (
  //   <div className="App">
  //     <h1>User List</h1>
  //     <ul>
  //       {data.map((user, index) => (
  //         <li key={index}>
  //           <strong>Name:</strong> {user.userName}, 
  //           <strong>Email:</strong> {user.userEmail}, 
  //           <strong>Matricula::</strong> {user.userEnrollment}, 
  //           <strong>Senha:</strong> {user.userPassword}, 
  //           <strong>Universidade</strong> {user.userPassword}
  //         </li>
  //       ))}
  //     </ul>
  //   </div>
  // );
}

export default App;
