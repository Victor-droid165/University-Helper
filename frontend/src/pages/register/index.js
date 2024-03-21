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
          value={user.userName}
          onChange={handleChange}
          fullWidth
          sx={{ marginTop: "5%", marginBottom: '4%', width: "80%",  marginLeft: "10%" }} // Adicione a propriedade sx diretamente no TextField
        />

        <TextField
          label="University"
          name="userUniversity"
          value={user.userUniversity}
          onChange={handleChange}
          fullWidth
          sx={{ marginBottom: '4%', width: "80%",  marginLeft: "10%" }} // Adicione a propriedade sx diretamente no TextField
        />

        <TextField
          label="Email"
          name="userEmail"
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
          value={user.userEnrollment}
          onChange={handleChange}
          fullWidth
          sx={{ marginBottom: '4%', width: "80%",  marginLeft: "10%" }} // Adicione a propriedade sx diretamente no TextField
        />
        
        <TextField
          label="Senha"
          name="userPassword"
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
