import * as React from 'react';
import Tabs from '@mui/material/Tabs';
import Tab from '@mui/material/Tab';
import Box from '@mui/material/Box';
import WarningIcon from '@mui/icons-material/Warning';
import DescriptionIcon from '@mui/icons-material/Description';
import ListUsers from './listUsers';
import ListNotes from '../notes/list/withEdit';
import { blue, red } from "@mui/material/colors";
import Warning from '../notes/creation/warning';
import Button from '@mui/material/Button';
import Modal from '@mui/material/Modal';
import AddIcon from '@mui/icons-material/Add';

export default function IconPositionTabs() {
  const [value, setValue] = React.useState(0);

  const handleChange = (event, newValue) => {
    setValue(newValue);
  };

  // Função para abrir o modal
  const [open, setOpen] = React.useState(false);
  const handleOpen = () => setOpen(true);
  const handleClose = () => setOpen(false);
  
  const getContent = (index) => {
    switch (index) {
      case 0:
        return <ListUsers />;
      case 1:
        return (
          <>
            <Box sx={{ mt: '0.7%', mr: '0.7%', display: 'flex', justifyContent: "flex-end"}}>
              <Button onClick={handleOpen} 
                      sx={{ 
                        bgcolor: 'red',
                        color: 'white',
                        fontFamily: 'Arial, sans-serif', // Escolha a família da fonte desejada
                        fontSize: '80%', // Ajuste o tamanho da fonte conforme necessário
                        fontWeight: 'bold', // Defina o peso da fonte
                        '&:hover': {
                          bgcolor: 'darkred', // Cor de fundo ao passar o mouse
                        }
                      }}>
                <AddIcon sx={{marginRight:1}}/> 
                NOVA ADVERTENCIA 
              </Button>
            </Box>
            <Modal
              open={open}
              onClose={handleClose}
              aria-labelledby="modal-modal-title"
              aria-describedby="modal-modal-description"
              sx={{
                display: 'flex',
                alignItems: 'center',
                justifyContent: 'center',
                overflow: 'auto',
              }}
            >
              <Box sx={{
                bgcolor: 'white',
                p: 4,
                borderRadius: 2,
                boxShadow: 24,
                maxWidth: '40%',
                maxHeight: '80%',
                overflow: 'auto'
              }}>
                <Warning />
              </Box>
            </Modal>
            <ListNotes />
          </>
        );
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
          <Tab icon={<DescriptionIcon />} label="Lista de Usuários" />
          <Tab icon={<WarningIcon />} label="Advertências" />
        </Tabs>
        <Box>
          {getContent(value)}
        </Box>
      </Box>
    </Box>
  );
}
