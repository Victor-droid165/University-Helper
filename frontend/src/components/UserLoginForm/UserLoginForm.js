import React, { useState } from 'react';
import { TextField, Box, Button } from '@mui/material';

const validateUser = async (user) => {
    const validationPromises = Object.keys(user).map(async (field) => {
        if (field === "userType") return null;

        const response = await fetch(`http://localhost:8081/${field}`, {
            method: 'POST',
            headers: {
                'Content-Type': 'application/json'
            },
            body: JSON.stringify({ value: user[field] })
        });

        if (!response.ok) {
            const errorData = await response.json();
            throw new Error(errorData.message);
        }

        return null;
    });

    return await Promise.all(validationPromises);;
}

const UserLoginForm = () => {
    const [user, setUser] = useState({
        userType: 'student',
        userName: '',
        userUniversity: '',
        userEnrollment: '',
        userEmail: '',
        userPassword: ''
    });

    const [errors, setErrors] = useState({});

    const handleChange = (e) => {
        setUser({
            ...user,
            [e.target.name]: e.target.value
        });
    };


    const handleSubmit = async (e) => {
        e.preventDefault();

        try {
            const validationResults = await validateUser(user);
            const validationErrors = validationResults.reduce((errors, error, index) => {
                if (error) {
                    errors[Object.keys(user)[index]] = error;
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
        value={user.userEmail}
        onChange={handleChange}
        fullWidth
        sx={styles.textField}
      />

      <TextField
        label="Senha"
        name="userPassword"
        value={user.userPassword}
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


