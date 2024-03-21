import React, { useState } from 'react';
import Page from "../../components/Page"

function Register() {
    return (
        <Page>
            <User></User>
        </Page>
    );
}

const User = () => {
    // const [form, setForm] = useState({
    //  userType
    //   email: '',
    //   enrollment: '',
    //   name: '',
    //   password: '',
    //   userType: '',
    //   university: ''
    // });
  
    const [user, setForm] = useState({
        userType: '',
        userName: '',
        userUniversity: '',
        userEnrollment: '',
        userEmail: '',
        userPassword: ''
    }); 

    const handleChange = (e) => {
      setForm({
        ...user,
        [e.target.name]: e.target.value
      });
    };
    
    const [errors, setErrors] = useState({});
  
    const handleSubmit = (e) => {
      e.preventDefault();
      let validationErrors = {};
    
      for (let field in user) {

        fetch('http://localhost:8081/' + field, {
          method: 'POST',
          headers: {
            'Content-Type': 'application/json'
          },
          body: JSON.stringify({ value: user[field] })
          
        })
        .then(response => response.json())
        .then(data => {
          // only test, I'll need to put extra things to check what's being returned by the API
            validationErrors[field] = data.message;
            validationErrors[field] = "deu ruim, lascou";
            console.log(data.message);
        })
        .catch((error) => {
          console.error('Error:', error);
          validationErrors[field] = "An error occurred while validating this field.";
        });
      }
    
      setErrors(validationErrors);
    };
  
    return (
      <form onSubmit={handleSubmit}>
        <label>
          Tipo de Usuário:
          <select name="userType" onChange={handleChange}>
            <option value="professor">Professor</option>
            <option value="student">Aluno</option>
          </select>
        </label>
        <label>
          Nome:
          <input type="text" name="userName" onChange={handleChange} />
        </label>
        <label>
          University:
          <input type="text" name="userUniversity" onChange={handleChange} />
        </label>
        <label>
          Email:
          <input type="text" name="userEmail" onChange={handleChange} />
          {errors.email && <p>{errors.email}</p>}
        </label>
        <label>
          Matrícula:
          <input type="text" name="userEnrollment" onChange={handleChange} />
        </label>
        <label>
          Senha:
          <input type="text" name="userPassword" onChange={handleChange} />
        </label>
        <button type="submit">Registrar</button>
      </form>
    );
  };

export default Register;