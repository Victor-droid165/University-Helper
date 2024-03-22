import React from 'react';
import { BrowserRouter as Router, Route, Routes } from 'react-router-dom';
import AppPageLayout from './layouts/AppPageLayout/AppPageLayout';
import HomePage from './pages/home';
import RegisterPage from './pages/register';
import LoginPage from './pages/login';

const CustomRoutes = () => (
  <Router>
    <AppPageLayout>
      <Routes>
        <Route index path="/" element={<HomePage />} />
        <Route path="/home" element={<HomePage />} />
        <Route path="/register" element={<RegisterPage />} />
        <Route path="/login" element={<LoginPage />} />
      </Routes>
    </AppPageLayout>
  </Router>
);

export default CustomRoutes;
