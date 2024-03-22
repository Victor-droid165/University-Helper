import React, { useState, useEffect } from 'react';
import { TextField, Box, Button } from '@mui/material';

const validateUser = async (userLoginInfo) => {
    const validationPromises = Object.keys(userLoginInfo).map(async (field) => {
        if (field === "userType") return null;

        const response = await fetch(`http://localhost:8081/${field}`, {
            method: 'POST',
            headers: {
                'Content-Type': 'application/json'
            },
            body: JSON.stringify({ value: userLoginInfo[field] })
        });

        if (!response.ok) {
            const errorData = await response.json();
            throw new Error(errorData.message);
        }
        return response.json();
    });

    return await Promise.all(validationPromises);
}

const UserLoginForm = () => {
    const [logInfo, setLog] = useState({
        userEmail: '',
        userPassword: '',
    }); 
    
    const [errors, setErrors] = useState ({
      emailError: '',
      passwordError: '',
    });

    useEffect(() => {
        if(errors.length == 0){
            const response = fetch(`http://localhost:8081/userLogin`, {
                method: 'POST',
                headers: {
                    'Content-Type': 'application/json'
                },
                body: JSON.stringify(logInfo)
            }).then(response => response.json()).then((json) => {
                console.log(json);
            }).catch((error) => {
                console.error('Error: ', error);
            });
        }
        console.log(errors)
    },[errors])

    const handleChange = (e) => {
        setLog({
          ...logInfo,
          [e.target.name]: e.target.value
        });
      };


    const handleSubmit = async (e) => {
        e.preventDefault();

        try {
            const validationResults = await validateUser(logInfo);
            const validationErrors = validationResults.reduce((errors, error, index) => {
                if (error.startsWith('Erro')) {
                    errors[Object.keys(logInfo)[index]] = error;
                }
                return errors;
            }, {});
            setErrors(validationErrors);
        } catch (error) {
            console.error('Erro:', error);
        }
    };

    return (
        <Box component="form" onSubmit={handleSubmit} sx={styles.form}>
      <TextField
        label="Email"
        name="userEmail"
        value={logInfo.userEmail}
        
        error = {Boolean(errors.userEmail)}
        helperText = {errors.userEmail}
        onChange={handleChange}
        fullWidth
        sx={styles.textField}
      />

      <TextField
        label="Senha"
        name="userPassword"
        value={logInfo.userPassword}
        error = {Boolean(errors.userPassword)}
        helperText = {errors.userPassword}
        onChange={handleChange}
        fullWidth
        sx={styles.textField}
      />
      <Button variant="contained" color="primary" type="submit" sx={styles.button}>Entrar</Button>
    </Box>
    );
};

const styles = {
    form: {
        width: '30%',
        height: '35%',
        position: 'absolute',
        top: '50%',
        left: '50%',
        transform: 'translate(-50%, -50%)',
        display: 'flex',
        alignItems: 'center',
        justifyContent: 'center',
        flexDirection: 'column',
        backgroundColor: 'white',
        border: '1px solid rgba(0, 0, 0, 0.1)',
        borderRadius: '8px',
        padding: '20px',
        boxShadow: '0px 4px 8px rgba(0, 0, 0, 0.1)',
    },
    textField: {
        marginBottom: '4%',
        width: '80%',
    },
    button: {
        width: '80%',
    },
};

export default UserLoginForm;


