import React, { useState, useEffect } from 'react';
import { Button, TextField, Typography, Container, Grid, IconButton, Select, MenuItem } from '@mui/material';
import CleaningServicesIcon from '@mui/icons-material/CleaningServices';
import { useApi } from '../../../hooks/useApi';
import { useLocation, useNavigate } from 'react-router-dom';

const Warning = ({ note }) => {
  const navigate = useNavigate();
  const location = useLocation();
  const api = useApi();
  const [title, setTitle] = useState('');
  const [warning, setWarning] = useState('');
  const [selectedUser, setSelectedUser] = useState('');
  const [dbUsersList, setDbUsersList] = useState([]);
  const isReadOnly = location.state?.readOnly ?? false;

  // Atualiza os estados quando o componente recebe uma nova 'note'
  useEffect(() => {
    if (note) {
      setTitle(note.title);
      setWarning(note.content); // Supondo que 'content' é a propriedade do aviso
    }
  }, [note]);

  useEffect(() => {
    const fetchData = async () => {
      try {
        const users = await api.getDBUsers();
        const newU = users.filter(user => user.dbIsDeleted !== true)
        setDbUsersList(newU);
      } catch (error) {
        console.error('Error fetching users:', error);
      }
    };
    fetchData();
  }, []);

  useEffect(() => {
    // Deixar isso aqui para atualizar quando o dbUsersList modificar
  }, [dbUsersList])

  const handleSave = async () => {
    note.content = warning;
    note.title = title;
    note.warnedUser = selectedUser;
    await api.updateNote(note);
    const prevPath = location.state?.prevPath;
    prevPath ? navigate(prevPath) : navigate('/');
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
          {isReadOnly ? "Visualizar Aviso" : "Editar Aviso"}
        </Typography>
        <form noValidate style={{ width: '100%' }}>
          {!isReadOnly && <Select
            value={selectedUser}
            onChange={(e) => setSelectedUser(e.target.value)}
            displayEmpty
            fullWidth
            style={{ marginBottom: '1rem' }}
          >
            <MenuItem value="">
              <em>Selecione um usuário</em>
            </MenuItem>
            {dbUsersList.map((dbUser) => (
              <MenuItem key={dbUser.dbUserId} value={dbUser}>
                {dbUser.dbUserName}
              </MenuItem>
            ))}
          </Select>
          }
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
            disabled={isReadOnly}
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
            disabled={isReadOnly}
          />
          {!isReadOnly &&
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
          }
        </form>
      </div>
    </Container>
  );
};

export default Warning;
