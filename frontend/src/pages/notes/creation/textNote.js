import React, { useState } from 'react';
import { Button, TextField, Typography, Container, Grid, IconButton, Checkbox, FormControlLabel } from '@mui/material';
import DeleteIcon from '@mui/icons-material/Delete';

const TextNote = () => {
  const [title, setTitle] = useState('');
  const [content, setContent] = useState('');
  const [isPublic, setIsPublic] = useState(false);

  const handleSave = () => {
    const now = new Date();
    console.log("Data e Hora:", now.toLocaleString());
    console.log("Título:", title);
    console.log("Conteúdo:", content);
    console.log("É público:", isPublic);
    // Aqui você pode adicionar lógica para salvar no backend, se necessário
  };

  const handleClear = () => {
    setTitle('');
    setContent('');
    setIsPublic(false);
  };

  return (
    <Container component="main" maxWidth='100%'>
      <div style={{ display: 'flex', flexDirection: 'column', alignItems: 'center' }}>
        <Typography component="h1" variant="h5" style={{ marginBottom: '1rem' }}>
          Criar Anotação
        </Typography>
        <form noValidate style={{ width: '100%' }}>
          <TextField
            variant="outlined"
            margin="normal"
            required
            fullWidth
            id="title"
            label="Título"
            name="title"
            autoFocus
            value={title}
            onChange={(e) => setTitle(e.target.value)}
            style={{ marginBottom: '1rem' }}
          />
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
            <Grid item xs={6} container justifyContent="flex-start">
              <FormControlLabel
                control={
                  <Checkbox
                    checked={isPublic}
                    onChange={(e) => setIsPublic(e.target.checked)}
                    name="isPublic"
                    color="primary"
                  />
                }
                label="Público"
              />
            </Grid>
            <Grid item xs={6} container justifyContent="flex-end">
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
                <DeleteIcon />
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

export default TextNote;