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
        userPassword: ''
    }); 

    const handleChange = (e) => {
      setUser({
        ...user,
        [e.target.name]: e.target.value
      });
    };
    
    const [errors, setErrors] = useState({});
  
    const handleSubmit = (e) => {
      e.preventDefault();
      let validationErrors = {};
    
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
        .then(data => {
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
          value={user.userName}
          onChange={handleChange}
          fullWidth
          sx={{ marginBottom: '4%', width: "80%" }} // Adicione a propriedade sx diretamente no TextField
        />

        <TextField
          label="Universidade"
          name="userUniversity"
          value={user.userUniversity}
          onChange={handleChange}
          fullWidth
          sx={{ marginBottom: '4%', width: "80%"}} // Adicione a propriedade sx diretamente no TextField
        />

        <TextField
          label="Email"
          name="userEmail"
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
          value={user.userEnrollment}
          onChange={handleChange}
          fullWidth
          sx={{ marginBottom: '4%', width: "80%" }} // Adicione a propriedade sx diretamente no TextField
        />
        
        <TextField
          label="Senha"
          name="userPassword"
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
