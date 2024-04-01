import Tabs from '@mui/material/Tabs';
import Tab from '@mui/material/Tab';
import Box from '@mui/material/Box';
import DescriptionIcon from '@mui/icons-material/Description';
import LightbulbIcon from '@mui/icons-material/Lightbulb';
import TextNote from './textNote';
import { blue, red } from "@mui/material/colors";
import Reminder from './reminder';
import { useState } from 'react';

export default function NoteCreationPage() {
  const [value, setValue] = useState(0);

  const handleChange = (event, newValue) => {
    setValue(newValue);
  };

  const getContent = (index) => {
    switch (index) {
      case 0:
        return <TextNote />;
      case 1:
        return <Reminder />;
      default:
        return null;
    }
  };

  return (
    <Box sx={{ p: 3, display: 'flex', justifyContent: 'center', alignItems: 'center' }}>
      <Box sx={{ flexGrow: 1, bgcolor: 'background.paper', borderRadius: '16px' }}>
        <Tabs
          value={value}
          onChange={handleChange}
          variant="fullWidth"
          textColor="primary"
          indicatorColor="primary"
          aria-label="icon position tabs example"
          sx={{
            '.MuiTabs-indicator': {
              backgroundColor: red[500], // Cor do indicador personalizada
            },
            '.MuiTabs-flexContainer': {
              bgcolor: blue[800], // Cor de fundo azul para os Tabs
            },
            '.MuiTab-root': {
              color: 'white', // Cor do texto dos Tabs
              '&.Mui-selected': { // Estilos para o Tab selecionado
                color: blue[50], // Cor do texto mais clara para o Tab selecionado
              },
            },
          }}
        >
          <Tab icon={<DescriptionIcon />} label="Texto Corrido" />
          <Tab icon={<LightbulbIcon />} label="Lembrete" />
        </Tabs>
        <Box sx={{ p: 5, borderRadius: '16px' }}>
          {getContent(value)}
        </Box>
      </Box>
    </Box>
  );
}
