import React from 'react';
import './App.css';

import router from './Routes/Routes.js';
import { RouterProvider } from 'react-router-dom';
import { AuthProvider } from './contexts/Auth/AuthProvider.js';

function App() {
  return (
    <AuthProvider>
      <RouterProvider router={router} />
    </AuthProvider>
  );
}

export default App;
