import React, { useState, useEffect } from 'react';
import { Button, TextField, Typography, Container, Grid, IconButton, Select, MenuItem } from '@mui/material';
import CleaningServicesIcon from '@mui/icons-material/CleaningServices';
import { mockDataTeam } from "../../../data/mockData.js";

const Warning = ({ note }) => {
  const [title, setTitle] = useState('');
  const [warning, setWarning] = useState('');
  const [selectedUser, setSelectedUser] = useState('');

  // Atualiza os estados quando o componente recebe uma nova 'note'
  useEffect(() => {
    if (note) {
      setTitle(note.title);
      setWarning(note.content); // Supondo que 'content' é a propriedade do aviso
      if (mockDataTeam && note.selectedUser) {
        const user = mockDataTeam.find(u => u.id === note.selectedUser);
        setSelectedUser(user ? user.name : '');
      }
    }
  }, [note, mockDataTeam]);

  const handleSave = () => {
    const now = new Date();
    console.log("Título:", title);
    console.log("Data e Hora:", now.toLocaleString());
    console.log("Usuário Selecionado:", selectedUser);
    console.log("Aviso:", warning);
    // Aqui você pode adicionar lógica para editar no backend
  };

  const handleClear = () => {
    setTitle('');
    setWarning('');
    setSelectedUser('');
  };

  return (
    <Container component="main" maxWidth='100%'>
      <div style={{ display: 'flex', flexDirection: 'column', alignItems: 'center' }}>
        <Typography component="h1" variant="h5" style={{ marginBottom: '1rem' }}>
          Editar Aviso
        </Typography>
        <form noValidate style={{ width: '100%' }}>
          <Select
            value={selectedUser}
            onChange={(e) => setSelectedUser(e.target.value)}
            displayEmpty
            fullWidth
            style={{ marginBottom: '1rem' }}
          >
            <MenuItem value="">
              <em>Selecione um usuário</em>
            </MenuItem>
            {mockDataTeam.map((user) => (
              <MenuItem key={user.id} value={user.name}>
                {user.name}
              </MenuItem>
            ))}
          </Select>
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
            id="warning"
            label="Aviso"
            name="warning"
            value={warning}
            onChange={(e) => setWarning(e.target.value)}
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

export default Warning;
