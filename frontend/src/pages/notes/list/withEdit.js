import React, { useState, useEffect } from 'react';
import Box from '@mui/material/Box';
import Grid from '@mui/material/Unstable_Grid2';
import NoteCardWithEdit from '../../../components/NoteCards/NoteCardWithEdit';
import { useAuth } from '../../../hooks/useAuth';
import { useApi } from '../../../hooks/useApi';

export default function ListNotesWithEdit() {
  const [data, setData] = useState([]);
  const api = useApi();
  const auth = useAuth();

  useEffect(() => {
    const fetchData = async () => {
      try {
        const users = await api.getDBUsers();
        const dbUserSession = users.find(user => user.dbUserEmail === auth.user.email);
        const jsonData = await api.getNotesByCreatorId(dbUserSession.dbUserId);
        setData(jsonData);
      } catch (error) {
        console.error('Error fetching notes:', error);
      }
    };
    fetchData();
  }, []);

  useEffect(() => {

  }, [data]);

  const updateData = (noteToRemove) => {
    const updatedData = data.filter(note => note.noteId !== noteToRemove.noteId);
    setData(updatedData);
  };

  return (
    <Box sx={{ flexGrow: 1, p: 2 }}>
      <Grid container spacing={2}>
        {data.map((currentNote, i) => (
          <Grid key={i} xs={12} sm={6} md={4} lg={3} minHeight={160}>
            <NoteCardWithEdit note={currentNote} updateData={updateData} />
          </Grid>
        ))}
      </Grid>
    </Box>
  );
}
