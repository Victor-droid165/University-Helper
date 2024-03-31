import React, { useState, useEffect } from 'react';
import Box from '@mui/material/Box';
import Grid from '@mui/material/Unstable_Grid2';
import NoteCardWithEdit from '../../../components/NoteCards/NoteCardWithEdit';
import { useAuth } from '../../../hooks/useAuth';
import { useApi } from '../../../hooks/useApi';

export default function ListNotesWithEdit() {
  const [data, setData] = useState([]);
  const api = useApi();
  const session = useAuth().user.email;

  useEffect(() => {
    const fetchData = async () => {
      try {
        const users = await api.getDBUsers();
        const dbUserSession = users.find(user => user.dbUserEmail === session);
        const response = await fetch('http://localhost:8081/api/notes/notes', {
          method: 'POST',
          headers: {
            'Content-Type': 'application/json',
          },
          body: JSON.stringify(dbUserSession.dbUserId.toString()),
        });
        const jsonData = await response.json();
        setData(jsonData);
      } catch (error) {
        console.error('Error fetching notes:', error);
      }
    };
    fetchData();
  }, []);

  return (
    <Box sx={{ flexGrow: 1, p: 2 }}>
      <Grid container spacing={2}>
        {data.map((currentNote, i) => (
          <Grid key={i} xs={12} sm={6} md={4} lg={3} minHeight={160}>
            <NoteCardWithEdit note={currentNote} />
          </Grid>
        ))}
      </Grid>
    </Box>
  );
}
