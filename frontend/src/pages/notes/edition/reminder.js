import React, { useState, useEffect } from 'react';
import { Button, TextField, Typography, Container, Grid, IconButton } from '@mui/material';
import CleaningServicesIcon from '@mui/icons-material/CleaningServices';

const Reminder = ({ note }) => {
  const [content, setContent] = useState('');

  // Atualiza os estados quando o componente recebe uma nova 'note'
  useEffect(() => {
    if (note) {
      setContent(note.content);
    }
  }, [note]);

  const handleSave = () => {
    const now = new Date();
    console.log("Data e Hora:", now.toLocaleString());
    console.log("Conteúdo:", content);
    // Aqui você pode adicionar lógica para atualizar no backend
  };

  const handleClear = () => {
    setContent('');
  };

  return (
    <Container component="main" maxWidth='100%'>
      <div style={{ display: 'flex', flexDirection: 'column', alignItems: 'center' }}>
        <Typography component="h1" variant="h5" style={{ marginBottom: '1rem' }}>
          Editar Lembrete
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
