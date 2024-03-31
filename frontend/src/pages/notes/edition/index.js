import * as React from 'react';
import Tabs from '@mui/material/Tabs';
import Tab from '@mui/material/Tab';
import Box from '@mui/material/Box';
import WarningIcon from '@mui/icons-material/Warning';
import DescriptionIcon from '@mui/icons-material/Description';
import LightbulbIcon from '@mui/icons-material/Lightbulb';
import TextNote from './textNote';
import Warning from './warning';
import Reminder from './reminder';
// Change to mockDataReminder or mockDataWarning to see all cases
import { blue, red } from "@mui/material/colors";
import { useLocation } from 'react-router-dom';

export default function EditNote() {
  const location = useLocation();
  const noteEditingNow = location.state.note;
  const [value, setValue] = React.useState(0);

  const handleChange = (event, newValue) => {
    setValue(newValue);
  };

  const getContent = (index) => {
    switch (noteEditingNow.noteType) {
      case 'Warning':
        return <Warning note={noteEditingNow}/>;
      case 'PlainText':
        return <TextNote note={noteEditingNow} />;
      case 'Reminder':
        return <Reminder note={noteEditingNow} />;
      default:
        return null;
    }
  };

  // Função para verificar se o tab deve ser visível
  const isTabVisible = (tabName) => {
    return noteEditingNow.type === tabName;
  };

  return (
    <Box sx={{ p: 3, display: 'flex', justifyContent: 'center', alignItems: 'center' }}>
      <Box sx={{ flexGrow: 1, bgcolor: 'background.paper', display: 'flex', flexDirection: 'column', borderRadius: '16px' }}>
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
          {isTabVisible('Warning') && <Tab icon={<WarningIcon />} label="Advertência" />}
          {isTabVisible('PlainText') && <Tab icon={<DescriptionIcon />} label="Texto Corrido" />}
          {isTabVisible('Reminder') && <Tab icon={<LightbulbIcon />} label="Lembrete" />}
        </Tabs>
        <Box sx={{ p: 5, borderRadius: '16px' }}>
          {getContent(value)}
        </Box>
      </Box>
    </Box>
  );
}
