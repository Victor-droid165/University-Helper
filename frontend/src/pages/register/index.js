import React, { useState } from 'react';
import ReactDOM from 'react-dom/client';
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

      
      fetch('http://localhost:8081/isRegistered', {
          method: 'Post',
          headers: {
          'Content-Type': 'application/json',
          },
          body: JSON.stringify({ value: user.userEmail })
          })
          .then(response => response.json())
          .then((json) => {
            if (json === 'Success') {

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

            } else {
              register = false;
              const Registered = () => {alert("Usuário já cadastrado no nosso sistema!\nFaça o login");}
              const root = ReactDOM.createRoot(document.getElementById('root'));
              root.render(<Registered />);
            }
            
          })

      setErrors ({
        userNameError: '',
        userUniversityError: '',
        userEmailError: '',
        userTypeError: 'Success',
        userEnrollmentError: '',
        userPasswordError: '',
      });
    };
  
    return (
      <Box component="form" onSubmit={handleSubmit} sx={{
        width: '35%',
        height: '70%',
        position: 'absolute',
        top: '50%',
        left: '50%',
        transform: 'translate(-50%, -50%)',
        display: 'flex',
        alignItems: 'center', // Alinha os itens verticalmente ao centro
        justifyContent: 'center', // Alinha os itens horizontalmente ao centro
        flexDirection: 'column',
        backgroundColor: 'white', // Adiciona fundo branco
        border: '1px solid rgba(0, 0, 0, 0.1)', // Adiciona uma borda com sombra
        borderRadius: '8px', // Adiciona um arredondamento de borda
        padding: '20px', // Adiciona um preenchimento interno
        boxShadow: '0px 4px 8px rgba(0, 0, 0, 0.1)', // Adiciona uma sombra
      }}>
        <TextField
          label="Nome"
          name="userName"

          error = {Boolean(errors.userNameError)}
          helperText={errors.userNameError}

          value={user.userName}
          onChange={handleChange}
          fullWidth
          sx={{ marginBottom: '4%', width: "80%" }} // Adicione a propriedade sx diretamente no TextField
        />

        <TextField
          label="Universidade"
          name="userUniversity"

          error = {Boolean(errors.userUniversityError)}
          helperText = {errors.userUniversityError}

          value={user.userUniversity}
          onChange={handleChange}
          fullWidth
          sx={{ marginBottom: '4%', width: "80%"}} // Adicione a propriedade sx diretamente no TextField
        />

        <TextField
          label="Email"
          name="userEmail"

          error = {Boolean(errors.userEmailError)}
          helperText = {errors.userEmailError}

          value={user.userEmail}
          onChange={handleChange}
          fullWidth
          sx={{ marginBottom: '4%', width: "80%" }} // Adicione a propriedade sx diretamente no TextField
        />
        <Select
          label="Tipo de Usuário"
          name="userType"
          value={user.userType}
          onChange={handleChange}
          fullWidth
          sx={{ marginBottom: '4%', width: "80%" }} // Adicione a propriedade sx diretamente no Select
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
          sx={{ marginBottom: '4%', width: "80%" }} // Adicione a propriedade sx diretamente no TextField
        />
        
        <TextField
          label="Senha"
          name="userPassword"

          error = {Boolean(errors.userPasswordError)}
          helperText = {errors.userPasswordError}

          value={user.userPassword}
          onChange={handleChange}
          fullWidth
          sx={{ marginBottom: '4%', width: "80%" }} // Adicione a propriedade sx diretamente no TextField
        />
        <Button variant="contained" color="primary" type="submit" sx={{ width: "80%" }}>Cadastrar</Button>
      </Box>
    );
};

export default Register;
