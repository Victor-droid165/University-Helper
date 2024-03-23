import React, { useState, useEffect } from 'react';
import ReactDOM from 'react-dom/client';
import { Avatar, Button, CssBaseline, TextField, FormControlLabel, Checkbox, Link, Grid, Box, Typography, Container, MenuItem } from '@mui/material';
import LockOutlinedIcon from '@mui/icons-material/LockOutlined';
import { createTheme, ThemeProvider } from '@mui/material/styles';
import Alert from '@mui/material/Alert';
import { useNavigate } from 'react-router-dom';


function Register() {
  return (
    <User></User>
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

  const [errors, setErrors] = useState({
    userNameError: '',
    userUniversityError: '',
    userEmailError: '',
    userTypeError: 'Success',
    userEnrollmentError: '',
    userPasswordError: '',
  });

  const [alerts, setAlerts] = useState([]);

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

  const navigate = useNavigate();
    
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
          setErrors(prevErrors => ({
            ...prevErrors,
            [field + 'Error']: (json === 'Success') ? '' : json,
          }))
          if (json === 'Success') {
            errors[field+'Error'] = '';
          } else {
            errors[field+'Error'] = json;
          }
      });
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
          var register = true
          for (let errorCheck in errors) {
            if (errors[errorCheck] !== '' && errors[errorCheck] !== 'Success') {
              register = false;
              break;
            } 
          }
          
          if (json === 'Success' && register) {

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
              navigate('/');

      } else if (json === 'Failure') {
          showAlert('error', "Usuário já cadastrado no nosso sistema!\nFaça o login!");
      }

    })


  };

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
              <Box component="form" noValidate onSubmit={handleSubmit} sx={{ mt: 3 }}>
                <Grid container spacing={2}>
                  <Grid item xs={12} sm={6}>
                    <TextField
                        autoComplete="given-name"
                        name="userName"
                        required
                        fullWidth
                        id="userName"
                        label="Nome"
                        autoFocus
                        error={Boolean(errors.userNameError)}
                        helperText={errors.userNameError}
                        value={user.userName}
                        onChange={handleChange}
                    />
                  </Grid>
                  <Grid item xs={12} sm={6}>
                    <TextField
                      required
                      fullWidth
                      id="userUniversity"
                      label="Universidade"
                      name="userUniversity"
                      autoComplete="university"
                      error={Boolean(errors.userUniversityError)}
                      helperText={errors.userUniversityError}
                      value={user.userUniversity}
                      onChange={handleChange}
                    />
                  </Grid>
                  <Grid item xs={12}>
                    <TextField
                      required
                      fullWidth
                      id="userEnrollment"
                      label="Matrícula"
                      name="userEnrollment"
                      autoComplete="enrollment"
                      error={Boolean(errors.userEnrollmentError)}
                      helperText={errors.userEnrollmentError}
                      value={user.userEnrollment}
                      onChange={handleChange}
                    />
                  </Grid>
                  <Grid item xs={12}>
                    <TextField
                      required
                      fullWidth
                      id="userEmail"
                      label="Email Address"
                      name="userEmail"
                      autoComplete="email"
                      error={Boolean(errors.userEmailError)}
                      helperText={errors.userEmailError}
                      value={user.userEmail}
                      onChange={handleChange}
                    />
                  </Grid>
                      <Grid item xs={12}>
                          <TextField
                              required
                              fullWidth
                              name="userPassword"
                              label="Password"
                              type="password"
                              id="userPassword"
                              autoComplete="new-password"
                              error={Boolean(errors.userPasswordError)}
                              helperText={errors.userPasswordError}
                              value={user.userPassword}
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

export default Register;
