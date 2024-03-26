import React from "react";
import { useState, useEffect } from "react";
import { createTheme, ThemeProvider } from "@mui/material/styles";
import { Box, Typography, MenuItem, Select, ListItemIcon, FormControl, IconButton } from "@mui/material";
import { DataGrid } from "@mui/x-data-grid";
import { mockDataTeam } from "../../data/mockData.js";
import AdminPanelSettingsOutlinedIcon from "@mui/icons-material/AdminPanelSettingsOutlined";
import LockOpenOutlinedIcon from "@mui/icons-material/LockOpenOutlined";
import SecurityOutlinedIcon from "@mui/icons-material/SecurityOutlined";
import DeleteIcon from "@mui/icons-material/Delete";
import { blue, red } from "@mui/material/colors";

const AdminPage = () => {
  const [rows, setRows] = useState([]);
  
  useEffect( () => {
      fetch('http://localhost:8081/api/users/users')
        .then(response=>response.json())
        .then(data => {
          console.log(data);
          const addId = data.map((user, index) => ({id: index + 1, ...user}));
          setRows(addId);
        })
        .catch(error => console.error('Error fetching data:', error))
    }, []);
  

  const theme = createTheme({
    components: {
      MuiDataGrid: {
        styleOverrides: {
          columnHeader: {
            backgroundColor: blue[800], // Cor de fundo do cabeçalho
            color: '#ffffff', // Cor do texto no cabeçalho
          },
        },
      },
    },
    palette: {
      black: {
        main: "#000000", // Cor primária
      },
      primary: {
        main: "#ffffff", // Cor primária
      },
      secondary: {
        main: blue[100], // Cor secundária
      },
      special: {
        main: blue["300"], // Cor secundária
      },
      error: {
        main: red[600], // Cor de erro
      },
      // Adicione outras cores conforme necessário
    },
  });

  const handleAccessChange = (id, newAccess) => {
    // Impede a alteração do nível de acesso se for 'admin'
    const currentAccess = rows.find(row => row.id === id).userType;
    if (currentAccess !== "Admin") {
      const newRows = rows.map((row) => {
        if (row.id === id) {
          return { ...row, userType: newAccess };
        }
        return row;
      });
      setRows(newRows);
    }
  };

  const handleDeleteRow = (id) => {
    const updatedRows = rows.filter(row => row.id !== id);
    setRows(updatedRows);
  };

  const columns = [
    { field: 'id', headerName: 'Id', headerAlign: 'center', flex: 0.5 },
    {
      field: 'userName',
      headerName: 'Nome',
      headerAlign: 'center',
      flex: 1,
    },
    {
      field: 'userUniversity',
      headerName: 'Universidade',
      type: 'number',
      headerAlign: 'center',
      align: 'left',
      flex: 1,
    },
    { field: 'userEnrollment', headerName: 'Matrícula', headerAlign: 'center', align: 'center', flex: 1 },
    { field: 'userEmail', headerName: 'Email', headerAlign: 'center', flex: 1 },
    {
      field: 'delete',
      headerName: 'Deletar',
      headerAlign: 'center',
      flex: 0.5,
      renderCell: (params) => (
        <IconButton onClick={() => handleDeleteRow(params.row.id)}>
          <DeleteIcon color="error" />
        </IconButton>
      ),
    },
    {
      field: 'userType',
      headerName: 'Cargo',
      headerAlign: 'center',
      flex: 1,
      renderCell: (params) => (
        <FormControl fullWidth>
          <Select
            value={params.row.userType}
            onChange={(event) => handleAccessChange(params.row.id, event.target.value)}
            displayEmpty
            size="small"
            sx={{
              display: 'flex',
              alignItems: 'center',
              backgroundColor: params.row.userType === 'admin'
                ? theme.palette.special.main
                : theme.palette.secondary.main,
              color: theme.palette.primary.contrastText,
            }}
          >
            <MenuItem value="Admin" disabled>
              <ListItemIcon>
                <AdminPanelSettingsOutlinedIcon fontSize="small" />
              </ListItemIcon>
              <Typography variant="body2">Admin</Typography>
            </MenuItem>
            <MenuItem value="Professor">
              <ListItemIcon>
                <SecurityOutlinedIcon fontSize="small" />
              </ListItemIcon>
              <Typography variant="body2">Professor</Typography>
            </MenuItem>
            <MenuItem value="Student">
              <ListItemIcon>
                <LockOpenOutlinedIcon fontSize="small" />
              </ListItemIcon>
              <Typography variant="body2">Aluno</Typography>
            </MenuItem>
          </Select>
        </FormControl>
      ),
    },
  ];

  return (
    <ThemeProvider theme={theme}>
      <Box padding={5} margin="0 auto" width={"95%"} justifyContent={"center"} alignItems={"center"}>
        <Box
          height="80vh"
          sx={{
            "& .MuiSelect-select": {
              display: 'flex',
              alignItems: 'center',
              pl: 1,
              pr: '24px',
            },
            "& .MuiDataGrid-root": {
              border: "none",
            },
            "& .MuiDataGrid-cell": {
              borderBottom: "none",
            },
            "& .name-column--cell": {
              color: theme.palette.black.main,
              backgroundColor: `${theme.palette.primary.main} !important`,
            },
            "& .MuiDataGrid-columnHeaders": {
              backgroundColor: `${theme.palette.primary.main} !important`,
              borderBottom: "none",
            },
            "& .MuiDataGrid-virtualScroller": {
              backgroundColor: theme.palette.primary.main,
            },
            "& .MuiDataGrid-footerContainer": {
              borderTop: "none",
              backgroundColor: theme.palette.primary.main,
            },
            "& .MuiCheckbox-root": {
              color: `${theme.palette.primary.main} !important`,
            },
          }}
        >
          <DataGrid rows={rows} columns={columns} autoPageSize/>
        </Box>
      </Box>
    </ThemeProvider>
  );
};

export default AdminPage;
