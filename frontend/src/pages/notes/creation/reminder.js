import React, { useState } from 'react';
import { Button, TextField, Typography, Container, Grid, IconButton } from '@mui/material';
import CleaningServicesIcon from '@mui/icons-material/CleaningServices';
import { useAuth } from '../../../hooks/useAuth';
import { useApi } from '../../../hooks/useApi';
import { useLocation, useNavigate } from 'react-router-dom';

const Reminder = () => {
  const [content, setContent] = useState('');
  const api = useApi();
  const auth = useAuth();
  const location = useLocation();
  const navigate = useNavigate();

  const handleSave = async () => {
    const noteID = await api.getNoteId("REM");
    const user = await api.getUserByField({ unique_key_name: "email", unique_key: auth.user.email })

    await api.registerNote({
      noteId: noteID,
      noteType: "Reminder",
      visibility: "Private",
      title: "",
      subject: "",
      content: content,
      creator: user,
    })
    const prevPath = location.state?.prevPath;
    prevPath ? navigate(prevPath) : navigate('/');
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
