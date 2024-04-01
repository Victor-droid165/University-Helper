import React from 'react';
import styles from './style.module.css';

import CircleNotificationsOutlinedIcon from '@mui/icons-material/CircleNotificationsOutlined';
import PeopleAltOutlinedIcon from '@mui/icons-material/PeopleAltOutlined';
import EditOutlinedIcon from '@mui/icons-material/EditOutlined';
import EditNoteOutlinedIcon from '@mui/icons-material/EditNoteOutlined';
import Grid from '@mui/material/Unstable_Grid2'; // Grid version 2
import DisplaySettingsOutlinedIcon from '@mui/icons-material/DisplaySettingsOutlined';

import { Link } from 'react-router-dom';

const HomePage = () => {
  return (
    <div className={styles.home}>
      <Grid >
        <div className={styles.content}>
          <small>Welcome to</small>
          <h1>University <br /> <span className={styles.title}>Helper</span></h1>
        </div>
      </Grid>
      <Grid container spacing={1} rowSpacing={1} >
        <Grid xs={4} >
          <Link to="note-list/user-warnings">
            <div className={styles.card}>
              <CircleNotificationsOutlinedIcon fontSize="large" />
              <strong><small>Central de notifcações</small></strong>
            </div>
          </Link>
        </Grid>
        <Grid xs={4}>
          <Link to="note-list">
            <div className={styles.card}>
              <EditNoteOutlinedIcon fontSize="large" />
              <strong><small>Gerenciar Anotações</small></strong>
            </div>
          </Link>
        </Grid>
        <Grid xs={4}>
          <Link to="note-creation">
            <div className={styles.card}>
              <EditOutlinedIcon fontSize="large" />
              <strong><small>Criar Anotações</small></strong>
            </div>
          </Link>
        </Grid>
        <Grid xs={4}>
          <Link to="note-creation">
            <div className={styles.card}>
              <PeopleAltOutlinedIcon fontSize="large" />
              <strong><small>Notas públicas</small></strong>
            </div>
          </Link>
        </Grid>
        <Grid xs={4}>
          <Link to="admin">
            <div className={styles.card}>
              <DisplaySettingsOutlinedIcon fontSize="large" />
              <strong><small>Tela de controle</small></strong>
            </div>
          </Link>
        </Grid>
      </Grid>
    </div>
  );
}

export default HomePage;