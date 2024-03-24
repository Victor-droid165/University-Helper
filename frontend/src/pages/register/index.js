import React, { useState, useEffect } from 'react';
import ReactDOM from 'react-dom/client';
import { Avatar, Button, CssBaseline, TextField, FormControlLabel, Checkbox, Link, Grid, Box, Typography, Container, MenuItem } from '@mui/material';
import LockOutlinedIcon from '@mui/icons-material/LockOutlined';
import { createTheme, ThemeProvider } from '@mui/material/styles';
import Alert from '@mui/material/Alert';
import { Form, useActionData, useLocation, useNavigate } from 'react-router-dom';
import { validateUser } from '../../components/UserLoginForm/UserLoginForm';
import { useAuth } from '../../auth';

function capitalize(str) {
  return str.charAt(0).toUpperCase() + str.slice(1);
}

function Register() {
  return (
    <User></User>
  );
}

const User = () => {
  const [user, setUser] = useState({
    type: 'student',
    name: '',
    university: '',
    enrollment: '',
    email: '',
    password: '',
  });

  const [errors, setErrors] = useState({
    userNameError: '',
    userUniversityError: '',
    userEmailError: '',
    userEnrollmentError: '',
    userPasswordError: '',
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
    setUser({
      ...user,
      [e.target.name]: e.target.value
    });
  };

  useEffect(() => {
    if (data) {
      if (data.validationErrors) {
        setErrors(data.validationErrors);
      }
      else if (data.alerts) {
        showAlert(...data.alerts);
        setErrors({
          userNameError: '',
          userUniversityError: '',
          userEmailError: '',
          userEnrollmentError: '',
          userPasswordError: '',
        });
      }
      else if (data.error)
        console.log(data.error);
      else {
        auth.login(data);
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
                Sign up
              </Typography>
              <Form method='post' action='/register'>
                <Box noValidate sx={{ mt: 3 }}>
                  <Grid container spacing={2}>
                    <Grid item xs={12} sm={6}>
                      <TextField
                        autoComplete="given-name"
                        name="name"
                        fullWidth
                        id="userName"
                        label="Nome"
                        autoFocus
                        error={Boolean(errors.userNameError)}
                        helperText={errors.userNameError}
                        value={user.name}
                        onChange={handleChange}
                      />
                    </Grid>
                    <Grid item xs={12} sm={6}>
                      <TextField
                        fullWidth
                        id="userUniversity"
                        label="Universidade"
                        name="university"
                        autoComplete="university"
                        error={Boolean(errors.userUniversityError)}
                        helperText={errors.userUniversityError}
                        value={user.university}
                        onChange={handleChange}
                      />
                    </Grid>
                    <Grid item xs={12}>
                      <TextField
                        fullWidth
                        id="userEnrollment"
                        label="Matrícula"
                        name="enrollment"
                        autoComplete="enrollment"
                        error={Boolean(errors.userEnrollmentError)}
                        helperText={errors.userEnrollmentError}
                        value={user.enrollment}
                        onChange={handleChange}
                      />
                    </Grid>
                    <Grid item xs={12}>
                      <TextField
                        fullWidth
                        id="userEmail"
                        label="Email Address"
                        name="email"
                        autoComplete="email"
                        error={Boolean(errors.userEmailError)}
                        helperText={errors.userEmailError}
                        value={user.email}
                        onChange={handleChange}
                      />
                    </Grid>
                    <Grid item xs={12}>
                      <TextField
                        fullWidth
                        name="password"
                        label="Password"
                        type="password"
                        id="userPassword"
                        autoComplete="new-password"
                        error={Boolean(errors.userPasswordError)}
                        helperText={errors.userPasswordError}
                        value={user.password}
                        onChange={handleChange}
                      />
                    </Grid>
                  </Grid>
                  <Button
                    type="submit"
                    fullWidth
                    variant="contained"
                    sx={{ mt: 3, mb: 2 }}
                  >
                    Sign Up
                  </Button>
                  <Grid container justifyContent="flex-end">
                    <Grid item>
                      <Link href="/login" variant="body2">
                        Already have an account? Sign in
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
}

export const registerAction = async ({ request }) => {
  const data = await request.formData();

  const registerInfoSubmission = {
    type: 'student',
    name: data.get('name'),
    university: data.get('university'),
    enrollment: data.get('enrollment'),
    email: data.get('email'),
    password: data.get('password'),
  }

  try {
    const validationResults = await validateUser(registerInfoSubmission);

    const validationErrors = validationResults.reduce((errors, error, index) => {
      if (error?.startsWith('Erro')) {
        errors['user' + capitalize(Object.keys(registerInfoSubmission)[index]) + 'Error'] = error;
      }
      return errors;
    }, {});

    if (Object.keys(validationErrors).length === 0) {

      const response = await fetch('http://localhost:8081/api/users/isRegistered', {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
        },
        body: JSON.stringify({ value: registerInfoSubmission.email })
      })
      const json = await response.json();
      const canRegister = json !== "Failure";

      const normalizedRegisterInfoSubmission = Object.keys(registerInfoSubmission).reduce((acc, key) => {
        const normalizedKey = 'user' + key.charAt(0).toUpperCase() + key.slice(1);
        acc[normalizedKey] = registerInfoSubmission[key];
        return acc;
      }, {});

      if (canRegister) {
        (async () => {
          await fetch('http://localhost:8081/api/users/register', {
            method: 'POST',
            headers: {
              'Content-Type': 'application/json',
            },
            body: JSON.stringify(normalizedRegisterInfoSubmission),
          });
        })();

      } else {
        return { alerts: ['error', "Usuário já cadastrado no nosso sistema!\nFaça o login!"] }
      }
    } else {
      return { validationErrors }
    }
  } catch (error) {
    return { error: 'Erro: ' + error }
  }

  return true;
}

export default Register;
