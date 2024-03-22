import React, { useState } from 'react';
import Page from "../../components/Page";
import { TextField, Box, Button, Select, MenuItem } from '@mui/material';

function Login() {
    return (
        <Page>
            <User></User>
        </Page>
    );
}

const User = () => {
    const [logInfo, setLog] = useState({
        email: '',
        password: '',
    }); 

    const [errors, setErrors] = useState ({
      emailError: '',
      passwordError: '',
    });

    const handleChange = (e) => {
      setLog({
        ...logInfo,
        [e.target.name]: e.target.value
      });
    };
    
  
    const handleSubmit = (e) => {
      e.preventDefault();

        fetch('http://localhost:8081/userEmail', {
          method: 'POST',
          headers: {
            'Content-Type': 'application/json'
          },
          body: JSON.stringify({ value: logInfo.email })
          
        })
        .then(response => response.json())
        .then((json) => {
            setErrors(prevErrors => ({
              ...prevErrors,
              emailError: (json === 'Success') ? '' : json,
            }))
            if (json === 'Success') {
              errors.emailError = '';
            } else {
              errors.emailError = json;
            }
        });

        fetch('http://localhost:8081/userPassword', {
          method: 'POST',
          headers: {
            'Content-Type': 'application/json'
          },
          body: JSON.stringify({ value: logInfo.password })
          
        })
        .then(response => response.json())
        .then((json) => {
            setErrors(prevErrors => ({
              ...prevErrors,
              passwordError: (json === 'Success') ? '' : json,
            }))
            if (json === 'Success') {
              errors.passwordError = '';
            } else {
              errors.passwordError = json;
            }
        });
        
      if ((errors.emailError === '') && (errors.passwordError === '')) {
      fetch('http://localhost:8081/userLogin', {
          method: 'POST',
          headers: {
            'Content-Type': 'application/json'
          },
          body: JSON.stringify(logInfo)
          
        })
        .then(response => response.json())
        .then((json) => {
            // ADD HERE WHAT'S GOING TO DO AFTER LOGIN
            console.log(json);
        })
        .catch((error) => {
          console.error('Error:', error);
        });
      }
    
    };
  
    return (
      <Box component="form" onSubmit={handleSubmit} sx={{
        width: '30%',
        height: '35%',
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
          label="Email"
          name="email"
          value={logInfo.email}
          
          error = {Boolean(errors.emailError)}
          helperText = {errors.emailError}

          onChange={handleChange}
          fullWidth
          sx={{ marginBottom: '4%', width: "80%" }} // Adicione a propriedade sx diretamente no TextField
        />
        
        <TextField
          label="Senha"
          name="password"
          value={logInfo.password}

          error = {Boolean(errors.passwordError)}
          helperText = {errors.passwordError}

          onChange={handleChange}
          fullWidth
          sx={{ marginBottom: '4%', width: "80%"}} // Adicione a propriedade sx diretamente no TextField
        />
        <Button variant="contained" color="primary" type="submit" sx={{ width: "80%" }}>Entrar</Button>
      </Box>
    );
};

export default Login;
