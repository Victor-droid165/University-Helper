import * as React from 'react';
import Box from '@mui/material/Box';
import Grid from '@mui/material/Unstable_Grid2';
import { mockDataNotes } from '../../../data/mockData';
import NoteCardWithEdit from '../../../components/NoteCards/NoteCardWithEdit';

export default function ListNotesWithEdit( {data = mockDataNotes} ) {
  return (
    <Box sx={{ flexGrow: 1, p: 2 }}>
      <Grid container spacing={2}>
        {data.map((currentNote, index) => (
          <Grid key={index} xs={12} sm={6} md={4} lg={3} minHeight={160}>
            <NoteCardWithEdit note={currentNote} />
          </Grid>
        ))}
      </Grid>
    </Box>
  );
}
