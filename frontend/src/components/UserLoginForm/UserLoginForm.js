import React, { useState, useEffect } from 'react';
import { Avatar, Button, CssBaseline, TextField, Link, Grid, Box, Typography, Container } from '@mui/material';
import LockOutlinedIcon from '@mui/icons-material/LockOutlined';
import { createTheme, ThemeProvider } from '@mui/material/styles';
import Alert from '@mui/material/Alert';
import { Form, useActionData, useLocation, useNavigate } from 'react-router-dom';
import { useAuth } from '../../hooks/useAuth';

function capitalize(str) {
    return str.charAt(0).toUpperCase() + str.slice(1);
}

export const validateUser = async (userInfo) => {
    const validationPromises = Object.keys(userInfo).map(async (field) => {
        if(field === 'type') return;

        const routeField = "api/users/validate" + capitalize(field);

        const response = await fetch(`http://localhost:8081/${routeField}`, {
            method: 'POST',
            headers: {
                'Content-Type': 'application/json'
            },
            body: JSON.stringify({ value: userInfo[field] })
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
    const [logInfo, setLogInfo] = useState({
        email: '',
        password: '',
    });

    const [errors, setErrors] = useState({
        emailError: '',
        passwordError: '',
    });

    const [alerts, setAlerts] = useState([]);
    const data = useActionData();
    const auth = useAuth();
    const navigate = useNavigate();
    const location = useLocation();

    const redirectPath = location.state?.path || '/';

    useEffect(() => {
        setErrors(errors);
    }, [errors]);

    useEffect(() => {
        if (alerts.length > 0) {
            const timer = setTimeout(() => {
                setAlerts(prevAlerts => prevAlerts.slice(1));
            }, 3000);

            return () => clearTimeout(timer);
        }
    }, [alerts]);

    const showAlert = (severity, message) => {
        setAlerts(prevAlerts => [...prevAlerts, { severity, message }]);
    };

    const handleChange = (e) => {
        setLogInfo({
            ...logInfo,
            [e.target.name]: e.target.value
        });
    };

    useEffect(() => {
        if (data) {
            if (data.validationErrors)
                setErrors(data.validationErrors);
            else if (data.alerts){
                showAlert(...data.alerts);
                setErrors({emailError: '', passwordError:''});
            }
            else if (data.error)
                console.log(data.error);
            else {
                auth.login(logInfo);
                navigate(redirectPath, { replace: true });
            }

        }
    }, [auth, data, navigate, redirectPath]);

    const styles = {
        form: {
            width: 'auto',
            height: 'auto',
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
            mt: 1,
            mb: 1,
        },
        button: {
            mt: 3,
            mb: 2,
        },
        alertContainer: {
            position: 'fixed',
            bottom: '2%',
            right: '1%',
            zIndex: 9999,
        },
    };

    return (
        <Box>
            <Box sx={styles.form}>
                <ThemeProvider theme={createTheme()}>
                    <Container component="main" maxWidth="xs">
                        <CssBaseline />
                        <Box
                            sx={{
                                display: 'flex',
                                flexDirection: 'column',
                                alignItems: 'center',
                            }}
                        >
                            <Avatar sx={{ m: 1, bgcolor: 'secondary.main' }}>
                                <LockOutlinedIcon />
                            </Avatar>
                            <Typography component="h1" variant="h5">
                                Sign in
                            </Typography>
                            <Form method='post' action='/login'>
                                <Box noValidate sx={{ mt: 1 }}>
                                    <TextField
                                        margin="normal"
                                        fullWidth
                                        id="email"
                                        label="Email Address"
                                        name="email"
                                        autoComplete="email"
                                        autoFocus
                                        sx={styles.textField}
                                        value={logInfo.email}
                                        onChange={handleChange}
                                        error={Boolean(errors.emailError)}
                                        helperText={errors.emailError}
                                    />
                                    <TextField
                                        margin="normal"
                                        fullWidth
                                        name="password"
                                        label="Password"
                                        type="password"
                                        id="password"
                                        autoComplete="current-password"
                                        sx={styles.textField}
                                        value={logInfo.password}
                                        onChange={handleChange}
                                        error={Boolean(errors.passwordError)}
                                        helperText={errors.passwordError}
                                    />
                                    <Button
                                        type="submit"
                                        fullWidth
                                        variant="contained"
                                        sx={{ ...styles.button, mt: 3, mb: 2 }}
                                    >
                                        Sign In
                                    </Button>
                                    <Grid container justifyContent="flex-end">
                                        <Grid item>
                                            <Link href="/register" variant="body2">
                                                Don't have an account? Sign Up
                                            </Link>
                                        </Grid>
                                    </Grid>
                                </Box>
                            </Form>
                        </Box>
                    </Container>
                </ThemeProvider>
            </Box>
            <Box sx={styles.alertContainer}>
                {alerts.map((alert, index) => (
                    <Alert key={index} severity={alert.severity} sx={{ my: 1 }}>
                        {alert.message}
                    </Alert>
                ))}
            </Box>
        </Box>
    );
};

export const loginAction = async ({ request }) => {
    const data = await request.formData();

    const logInfoSubmission = {
        email: data.get('email'),
        password: data.get('password')
    }

    try {
        const validationResults = await validateUser(logInfoSubmission);
        const validationErrors = validationResults.reduce((errors, error, index) => {
            if (error.startsWith('Erro')) {
                errors[Object.keys(logInfoSubmission)[index] + 'Error'] = error;
            }
            return errors;
        }, {});

        if (Object.keys(validationErrors).length === 0) {
            const response = await fetch(`http://localhost:8081/api/users/validateLogin`, {
                method: 'POST',
                headers: {
                    'Content-Type': 'application/json',
                },
                body: JSON.stringify(logInfoSubmission)
            });
            const json = await response.json();
            if (!json) {
                return { alerts: ['error', 'Login e/ou senha inválidos'] };
            }
        } else {
            return { validationErrors }
        }
    } catch (error) {
        return { error: 'Erro: ' + error }
    }

    return true;
}

export default UserLoginForm;
