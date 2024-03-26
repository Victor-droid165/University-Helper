import React, { useState } from 'react';
import { Button, TextField, Typography, Container, Grid, IconButton, Select, MenuItem } from '@mui/material';
import DeleteIcon from '@mui/icons-material/Delete';
import { mockDataTeam } from "../../data/mockData.js";

const Notice = () => {
  const [title, setTitle] = useState('');
  const [notice, setNotice] = useState('');
  const [selectedUser, setSelectedUser] = useState('');
  const [rows, setRows] = useState(mockDataTeam);

  const handleSave = () => {
    const now = new Date();
    console.log("Data e Hora:", now.toLocaleString());
    console.log("Usuário Selecionado:", selectedUser);
    console.log("Título:", title);
    console.log("Aviso:", notice);
    // Aqui você pode adicionar lógica para salvar no backend, se necessário
  };

  const handleClear = () => {
    setTitle('');
    setNotice('');
    setSelectedUser('');
  };

  return (
    <Container component="main" maxWidth='100%'>
      <div style={{ display: 'flex', flexDirection: 'column', alignItems: 'center' }}>
        <Typography component="h1" variant="h5" style={{ marginBottom: '1rem' }}>
          Criar Aviso
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
            {rows.map((user) => (
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
            id="notice"
            label="Aviso"
            name="notice"
            value={notice}
            onChange={(e) => setNotice(e.target.value)}
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

export default Notice;
