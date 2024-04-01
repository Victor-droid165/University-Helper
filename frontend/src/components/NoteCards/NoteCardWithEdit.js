import Box from '@mui/material/Box';
import Card from '@mui/material/Card';
import CardActions from '@mui/material/CardActions';
import CardContent from '@mui/material/CardContent';
import Button from '@mui/material/Button';
import Typography from '@mui/material/Typography';
import WarningIcon from '@mui/icons-material/Warning';
import DescriptionIcon from '@mui/icons-material/Description';
import LightbulbIcon from '@mui/icons-material/Lightbulb';
import { IconButton, Grid } from '@mui/material';
import DeleteIcon from "@mui/icons-material/Delete";
import { useNavigate, useLocation } from 'react-router-dom';
import { useApi } from '../../hooks/useApi';


export default function NoteCardWithEdit({ note, updateData }) {
  const location = useLocation();
  const navigate = useNavigate();
  const api = useApi();

  const handleEditClick = () => {
    navigate('/note-edition', { state: { note, prevPath: location.pathname} });
  };

  const handleRemoveClick = async () => {
    await api.removeNote(note);
    updateData(note);
  };

  // Define os ícones e as cores para cada tipo de anotação
  const tagStyles = {
    Warning: { icon: <WarningIcon />, color: '#f32f2f' }, // Um tom mais escuro de vermelho
    PlainText: { icon: <DescriptionIcon />, color: '#40de00' }, // Um tom mais escuro de verde
    Reminder: { icon: <LightbulbIcon />, color: 'orange' }, // Um tom mais escuro de amarelo
  };

  // Escolhe o ícone e a cor com base no tipo de anotação
  const { icon, color } = tagStyles[note.noteType] || { icon: null, color: 'grey' };

  return (
    <Box sx={{ minWidth: 275, position: 'relative', mb: 2 }}>
      <Box
        sx={{
          bgcolor: color,
          position: 'absolute',
          top: 16,
          left: 16,
          px: 1,
          py: 0.5,
          borderRadius: 2,
          color: 'white',
          display: 'flex',
          alignItems: 'center',
          gap: 0.5,
          zIndex: 1,
        }}
      >
        {icon}
        <Typography variant="caption" sx={{ fontWeight: 'bold' }}>
          {(note.noteType).toUpperCase()}
        </Typography>
      </Box>
      <Card variant="outlined" sx={{ pt: 7 }}>
        <CardContent>
          <Typography
            sx={{
              fontSize: 14,
              color: 'text.secondary',
              gutterBottom: true,
              whiteSpace: 'nowrap',
              overflow: 'hidden',
              textOverflow: 'ellipsis',
              maxWidth: '100%',
            }}
          >
            {note.noteType === 'Reminder' ? note.content : note.title}
          </Typography>
        </CardContent>
        <CardActions>
          <Grid container justifyContent="flex-end">
            <Grid item>
              <IconButton size="small" onClick={handleRemoveClick}>
                <DeleteIcon color="error" />
              </IconButton>
            </Grid>
            <Grid item>
              <Button size="small" onClick={handleEditClick}>Editar</Button>
            </Grid>
          </Grid>
        </CardActions>
      </Card>
    </Box>
  );
}
