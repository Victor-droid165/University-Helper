import React, { useState, useEffect } from 'react';
import { Button, TextField, Typography, Container, Grid, IconButton } from '@mui/material';
import CleaningServicesIcon from '@mui/icons-material/CleaningServices';
import { useAuth } from '../../../hooks/useAuth';
import { useApi } from '../../../hooks/useApi';

const Reminder = () => {
  const [content, setContent] = useState('');
  const api = useApi();
  const session = useAuth().user.email;

  useEffect(() => {
    const fetchDBUserSession = async () => {
      const users = await api.getDBUsers();
      const dbUserSession = users.filter(user => user.dbUserEmail === session);
      console.log(dbUserSession[0]);
      return dbUserSession[0];
    }
  }, [api, session]);

  const getId = async (idType) => {
    try {
      const response = await fetch('http://localhost:8081/api/notes/getId', {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
        },
        body: JSON.stringify({value: idType}),
      });
      const data = await response.json();
      console.log(data);
      return data;
    } catch (error) {
      console.error('Error updating user:', error);
    }
  };

  const handleSave = async () => {
    const now = new Date();
    console.log("Data e Hora:", now.toLocaleString());
    console.log("Conteúdo:", content);
    const noteID = await getId("REM");
    const users = await api.getDBUsers();
    const dbUserSession = users.filter(user => user.dbUserEmail === session);
    console.log(dbUserSession[0]);

    const user = {
      userName: dbUserSession[0].dbUserName,
      userEmail: dbUserSession[0].dbUserEmail,
      userPassword: dbUserSession[0].dbUserPassword,
      userType: dbUserSession[0].dbUserType,
      userEnrollment: dbUserSession[0].dbUserEnrollment,
      userUniversity: dbUserSession[0].dbUserUniversity,
    }

    // Aqui você pode adicionar lógica para salvar no backend
    await fetch('http://localhost:8081/api/notes/registerNote', {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
      },
      body: JSON.stringify({ 
        noteId: noteID,
        noteType: "Reminder",
        visibility: "Private",
        title: "",
        subject: "",
        content: content,
        creator: user,
       }),
    });
  };

  const handleClear = () => {
    setContent('');
  };

  return (
    <Container component="main" maxWidth='100%'>
      <div style={{ display: 'flex', flexDirection: 'column', alignItems: 'center' }}>
        <Typography component="h1" variant="h5" style={{ marginBottom: '1rem' }}>
          Criar Lembrete
        </Typography>
        <form noValidate style={{ width: '100%' }}>
          <TextField
            variant="outlined"
            margin="normal"
            required
            fullWidth
            multiline
            rows={8}
            id="content"
            label="Conteúdo"
            name="content"
            value={content}
            onChange={(e) => setContent(e.target.value)}
            style={{ marginBottom: '1rem' }}
          />
          <Grid container spacing={2}>
            <Grid item xs={12} container justifyContent="flex-end">
              <IconButton
                type="button"
                onClick={handleClear}
                sx={{
                  color: 'white',
                  backgroundColor: 'error.main',
                  '&:hover': {
                    backgroundColor: 'error.dark',
                  },
                  borderRadius: '15%',
                  marginTop: '1rem',
                }}
              >
                <CleaningServicesIcon />
              </IconButton>
              <Button
                type="button"
                variant="contained"
                color="primary"
                onClick={handleSave}
                sx={{ marginTop: '1rem', marginLeft: '1rem' }}
              >
                Salvar
              </Button>
            </Grid>
          </Grid>
        </form>
      </div>
    </Container>
  );
};

export default Reminder;
