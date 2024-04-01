import Box from '@mui/material/Box';
import Grid from '@mui/material/Unstable_Grid2';
import NoteCardReadOnly from '../../../components/NoteCards/NoteCardReadOnly';
import { useApi } from '../../../hooks/useApi';
import { useState, useEffect } from 'react';
import { useAuth } from '../../../hooks/useAuth';

export default function ListNotesReadOnly() {
  const [data, setData] = useState([]);
  const api = useApi();
  const auth = useAuth();

  useEffect(() => {
    const fetchData = async () => {
      try {
        const users = await api.getDBUsers();
        const dbUserSession = users.find(user => user.dbUserEmail === auth.user.email);
        const jsonData = await api.getUserWarnings(dbUserSession.dbUserId);
        setData(jsonData);
      } catch (error) {
        console.error('Error fetching notes:', error);
      }
    };
    fetchData();
  }, []);

  useEffect(() => {

  }, [data]);

  return (
    <Box sx={{ flexGrow: 1, p: 2 }}>
      <Grid container spacing={2}>
        {data.map((currentNote, index) => (
          <Grid key={index} xs={12} sm={6} md={4} lg={3} minHeight={160}>
            <NoteCardReadOnly note={currentNote} />
          </Grid>
        ))}
      </Grid>
    </Box>
  );
}
