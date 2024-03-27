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
      fetch('http://localhost:8081/api/users/usersDB')
        .then(response=>response.json())
        .then(data => {
          console.log(data);
          //const addId = data.map((user, index) => ({id: index + 1, validate: "teste", ...user}));
          //setRows(addId);
          setRows(data)
        })
        .catch(error => console.error('Error fetching data:', error))
    }, []);
  
  const updateAny = (field, newValue, match, matchValue) => {
      const data = { field, newValue, match, matchValue };
      fetch('http://localhost:8081/api/users/updateAny', {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
        },
        body: JSON.stringify(data),
      })
        .then(response => {
          if (!response.ok) {
            throw new Error('Failed to update user');
          }
          console.log(response);
          return response.json();
        })
        .then(() => {
          console.log(data);
        })
        .catch(error => {
          console.error('Error updating user:', error);
        });
    };

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

  const handleAccessChange = (id, newAccess, email) => {
    // Impede a alteração do nível de acesso se for 'admin'
    const currentAccess = rows.find(row => row.id === id).dbUserType;
    console.log(newAccess);
    if (currentAccess !== "Admin") {
      const newRows = rows.map((row) => {
        if (row.id === id) {
          return { ...row, dbUserType: newAccess };
        }
        return row;
      });
      updateAny("type", newAccess, "email", email)
      setRows(newRows);
    }
  };

  
  const handleDeleteRow = (id) => {
    fetch('http://localhost:8081/api/users/deleteUser', {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
      },
      body: JSON.stringify({ value: rows.find(row => row.id === id).dbUserId.toString() }),
    })
      .then(response => {
        if (!response.ok) {
          throw new Error('Failed to delete user');
        }
        window.location.reload()
      })
      .catch(error => console.error('Error deleting user:', error));
  };

  const columns = [
    { field: 'dbUserId', headerName: 'Id', headerAlign: 'center', flex: 0.5 },
    {
      field: 'dbUserName',
      headerName: 'Nome',
      headerAlign: 'center',
      flex: 1,
    },
    {
      field: 'dbUserUniversity',
      headerName: 'Universidade',
      type: 'number',
      headerAlign: 'center',
      align: 'left',
      flex: 1,
    },
    { field: 'dbUserEnrollment', headerName: 'Matrícula', headerAlign: 'center', align: 'center', flex: 1 },
    { field: 'dbUserEmail', headerName: 'Email', headerAlign: 'center', flex: 1 },
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
      field: 'validate', headerName: "Validação", headerAlign: 'center', align: 'center', flex: 1
      
    },
    {
      field: 'dbUserType',
      headerName: 'Cargo',
      headerAlign: 'center',
      flex: 1,
      renderCell: (params) => (
        <FormControl fullWidth>
          <Select
            value={params.row.dbUserType}
            onChange={(event) => handleAccessChange(params.row.id, event.target.value, params.row.dbUserEmail)}
            displayEmpty
            size="small"
            sx={{
              display: 'flex',
              alignItems: 'center',
              backgroundColor: params.row.dbUserType === 'admin'
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
          <DataGrid   rows={rows} 
                      columns={columns} 
                      getRowId={(row) => row.dbUserId}
                      autoPageSize/>
        </Box>
      </Box>
    </ThemeProvider>
  );
};

export default AdminPage;
