import React, { useState, useEffect } from 'react';
import { Button, TextField, Typography, Container, Grid, IconButton, Select, MenuItem } from '@mui/material';
import CleaningServicesIcon from '@mui/icons-material/CleaningServices';
import { useApi } from '../../../hooks/useApi';
import { useAuth } from '../../../hooks/useAuth';
import { useLocation, useNavigate } from 'react-router-dom';

const Warning = () => {
  const navigate = useNavigate();
  const location = useLocation();
  const api = useApi();
  const auth = useAuth();
  const [title, setTitle] = useState('');
  const [warning, setWarning] = useState('');
  const [selectedUser, setSelectedUser] = useState('');
  const [dbUsersList, setDbUsersList] = useState([]);

  useEffect(() => {
    const fetchData = async () => {
      const users = await api.getDBUsers();
      const newU = users.filter(user => user.dbIsDeleted !== true)
      setDbUsersList(newU);
    };
    fetchData();
  }, []);

  const handleSave = async () => {
    const noteID = await api.getNoteId("WAR");
    const creator = await api.getUserByField({ unique_key_name: "email", unique_key: auth.user.email });

    await api.registerNote({
      noteId: noteID,
      noteType: "Warning",
      visibility: "Private",
      title: title,
      subject: "",
      content: warning,
      creator: creator,
      warnedUser: selectedUser
    })
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
      <div style={{ marginTop: "2%", display: 'flex', flexDirection: 'column', alignItems: 'center' }}>
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
            {dbUsersList.map((user) => (
              <MenuItem key={user.dbUserName} value={user}>
                {user.dbUserName}
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
