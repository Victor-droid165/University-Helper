import React, { useState } from 'react';
import Page from "../../components/Page";
import { TextField, Box, Button, Select, MenuItem } from '@mui/material';

function Register() {
    return (
        <Page>
            <User></User>
        </Page>
    );
}

const User = () => {
    const [user, setUser] = useState({
        userType: 'student',
        userName: '',
        userUniversity: '',
        userEnrollment: '',
        userEmail: '',
        userPassword: '',
    });

    const [errors, setErrors] = useState ({
      userNameError: '',
      userUniversityError: '',
      userEmailError: '',
      userTypeError: 'Success',
      userEnrollmentError: '',
      userPasswordError: '',
    });

    const handleChange = (e) => {
      setUser({
        ...user,
        [e.target.name]: e.target.value
      });
    };
      
    const handleSubmit = (e) => {
      e.preventDefault();
    
      for (let field in user) {
        if (field === "userType") continue;

        fetch('http://localhost:8081/' + field, {
          method: 'POST',
          headers: {
            'Content-Type': 'application/json'
          },
          body: JSON.stringify({ value: user[field] })
          
        })
        .then(response => response.json())
        .then((json) => {
            console.log(json);
            setErrors(prevErrors => ({
              ...prevErrors,
              [field + 'Error']: (json === 'Success') ? '' : json,
            }))
        });
      }

      let register = true;
      for (let errorCheck in errors) {
        if (errors[errorCheck] !== '' && errors[errorCheck] !== 'Success') {
          register = false;
          break;
        }
      }

      if (register === true) {
          fetch('http://localhost:8081/register', {
          method: 'Post',
          headers: {
          'Content-Type': 'application/json',
          },
            body: JSON.stringify(user),
          })
          .then(response => response.json())
          .then(data => {
            console.log('Success:', data);
          })
          .catch((error) => {
            console.error('Error:', error);
          });

        setErrors ({
          userNameError: '',
          userUniversityError: '',
          userEmailError: '',
          userTypeError: 'Success',
          userEnrollmentError: '',
          userPasswordError: '',
        });

      }
    };
  
    return (
      <Box component="form" onSubmit={handleSubmit} sx={{
        width: '50%',
        height: '70%',
        position: 'absolute',
        top: '50%',
        left: '50%',
        transform: 'translate(-50%, -50%)',
        display: 'flex',
        flexDirection: 'column',
      }}>
        <TextField
          label="Nome"
          name="userName"

          error = {Boolean(errors.userNameError)}
          helperText={errors.userNameError}

          value={user.userName}
          onChange={handleChange}
          fullWidth
          sx={{ marginTop: "5%", marginBottom: '4%', width: "80%",  marginLeft: "10%" }} // Adicione a propriedade sx diretamente no TextField
        />

        <TextField
          label="Universidade"
          name="userUniversity"

          error = {Boolean(errors.userUniversityError)}
          helperText = {errors.userUniversityError}

          value={user.userUniversity}
          onChange={handleChange}
          fullWidth
          sx={{ marginBottom: '4%', width: "80%",  marginLeft: "10%" }} // Adicione a propriedade sx diretamente no TextField
        />

        <TextField
          label="Email"
          name="userEmail"

          error = {Boolean(errors.userEmailError)}
          helperText = {errors.userEmailError}

          value={user.userEmail}
          onChange={handleChange}
          fullWidth
          sx={{ marginBottom: '4%', width: "80%",  marginLeft: "10%" }} // Adicione a propriedade sx diretamente no TextField
        />
        <Select
          label="Tipo de Usuário"
          name="userType"
          value={user.userType}
          onChange={handleChange}
          fullWidth
          sx={{ marginBottom: '4%', width: "80%",  marginLeft: "10%" }} // Adicione a propriedade sx diretamente no Select
        >
          <MenuItem value="student">Aluno</MenuItem>
          <MenuItem value="professor">Professor</MenuItem>
        </Select> 

        <TextField
          label="Matricula"
          name="userEnrollment"

          error = {Boolean(errors.userEnrollmentError)}
          helperText = {errors.userEnrollmentError}

          value={user.userEnrollment}
          onChange={handleChange}
          fullWidth
          sx={{ marginBottom: '4%', width: "80%",  marginLeft: "10%" }} // Adicione a propriedade sx diretamente no TextField
        />
        
        <TextField
          label="Senha"
          name="userPassword"

          error = {Boolean(errors.userPasswordError)}
          helperText = {errors.userPasswordError}

          value={user.userPassword}
          onChange={handleChange}
          fullWidth
          sx={{ marginBottom: '4%', width: "80%",  marginLeft: "10%" }} // Adicione a propriedade sx diretamente no TextField
        />
        <Button variant="contained" color="primary" type="submit" sx={{ width: "80%",  marginLeft: "10%" }}>Registrar</Button>
      </Box>
    );
};

export default Register;
