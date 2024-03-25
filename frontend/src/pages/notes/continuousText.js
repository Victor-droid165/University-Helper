import React, { useState } from 'react';
import { Container, TextField, Button, FormControl } from '@mui/material';

function TextNote() {
  const [note, setNote] = useState('');

  const handleNoteChange = (event) => {
    setNote(event.target.value);
  };

  const saveNote = () => {
    // Aqui você pode adicionar a lógica para salvar a nota
    console.log('Nota salva:', note);
  };

  return (
    <>
      <FormControl fullWidth>
        <Container maxWidth={false} style={{ backgroundColor: '#fff', padding: '20px' }}>
            <TextField
            label="Anotação"
            multiline
            rows={4}
            fullWidth
            variant="outlined"
            value={note}
            onChange={handleNoteChange}
            margin="normal"
            />
            <Button variant="contained" color="primary" onClick={saveNote}>
            Salvar
            </Button>
        </Container>
      </FormControl>
    </>
  );
}

export default TextNote;
