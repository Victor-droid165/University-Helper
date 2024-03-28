import React, { useState, useEffect } from 'react';
import { Avatar, Button, CssBaseline, TextField, Select, Link, Grid, Box, Typography, Container, MenuItem } from '@mui/material';
import LockOutlinedIcon from '@mui/icons-material/LockOutlined';
import { createTheme, ThemeProvider } from '@mui/material/styles';
import Alert from '@mui/material/Alert';
import { Form, useLocation, useNavigate } from 'react-router-dom';
import { useAuth } from '../../hooks/useAuth';
import { useApi } from '../../hooks/useApi';
import { validateUser } from '../../utils/utils';

function Register() {
  return (
    <User></User>
  );
}

const User = () => {
  const [user, setUser] = useState({
    type: 'Student',
    name: '',
    university: '',
    enrollment: '',
    email: '',
    password: '',
  });

  const [errors, setErrors] = useState({
    nameError: '',
    universityError: '',
    emailError: '',
    enrollmentError: '',
    passwordError: '',
  });

  const [alerts, setAlerts] = useState([]);

  const auth = useAuth();
  const api = useApi();
  const navigate = useNavigate();
  const location = useLocation();

  const redirectPath = location.state?.path || '/';

  useEffect(() => {
    setErrors(errors);
    if (alerts.length > 0) {
      const timer = setTimeout(() => {
        setAlerts(prevAlerts => prevAlerts.slice(1));
      }, 3000);

      return () => clearTimeout(timer);
    }
  }, [alerts, errors]);

  const showAlert = (severity, message) => {
    setAlerts(prevAlerts => [...prevAlerts, { severity, message }]);
  };

  const handleChange = (e) => {
    setUser({
      ...user,
      [e.target.name]: e.target.value
    });
  };

  const handleSubmit = (e) => {
    e.preventDefault();
    handleRegister();
  };

  const handleRegister = async () => {
    try {
      const validationErrors = await validateUser(api, user);

      if (Object.keys(validationErrors).length === 0) {

        const isRegistered = await api.isRegistered(user);
        const canRegister = isRegistered !== "Failure";

        const normalizedUserInfo = Object.keys(user).reduce((acc, key) => {
          const normalizedKey = 'user' + key.charAt(0).toUpperCase() + key.slice(1);
          acc[normalizedKey] = user[key];
          return acc;
        }, {});

        if (canRegister) {
          await api.registerUser(normalizedUserInfo);
          auth.login({ email: user.email });
          navigate(redirectPath, { replace: true });

        } else {
          showAlert('error', "Usuário já cadastrado no nosso sistema!\nFaça o login!");
          setErrors({
            nameError: '',
            universityError: '',
            emailError: '',
            enrollmentError: '',
            passwordError: '',
          });
        }
      } else {
        setErrors(validationErrors);
      }
    } catch (error) {
      console.log(error);
    }
  }

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
              <Form method='post' onSubmit={handleSubmit}>
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
                        error={Boolean(errors.nameError)}
                        helperText={errors.nameError}
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
                        error={Boolean(errors.universityError)}
                        helperText={errors.universityError}
                        value={user.university}
                        onChange={handleChange}
                      />
                    </Grid>
                    <Grid item xs={12}>
                      <Select
                        fullWidth
                        id="userType"
                        name="type"
                        autoComplete="userType"
                        value={user.type}
                        onChange={handleChange}
                      >
                        <MenuItem value="Student">Aluno</MenuItem>
                        <MenuItem value="Professor">Professor</MenuItem>
                      </Select>
                    </Grid>
                    <Grid item xs={12}>
                      <TextField
                        fullWidth
                        id="userEnrollment"
                        label="Matrícula"
                        name="enrollment"
                        autoComplete="enrollment"
                        error={Boolean(errors.enrollmentError)}
                        helperText={errors.enrollmentError}
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
                        error={Boolean(errors.emailError)}
                        helperText={errors.emailError}
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
                        error={Boolean(errors.passwordError)}
                        helperText={errors.passwordError}
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

export default Register;
