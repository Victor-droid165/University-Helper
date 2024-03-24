import React from 'react';
import { Route, createBrowserRouter, createRoutesFromElements } from 'react-router-dom';
import AppPageLayout from './layouts/AppPageLayout/AppPageLayout';
import HomePage from './pages/home';
import RegisterPage, { registerAction } from './pages/register';
import LoginPage from './pages/login';
import { loginAction, logoutAction } from './components/UserLoginForm/UserLoginForm';

const router = createBrowserRouter(
  createRoutesFromElements(
    <Route path='/' element={<AppPageLayout />}>
      <Route index element={<HomePage />} />
      <Route path="home" element={<HomePage />} />
      <Route path="register" element={<RegisterPage />} action={registerAction}/>
      <Route path="login" element={<LoginPage />} action={loginAction}/>
      <Route path="logout" action={logoutAction}/>

      {/*<Route path='protected' element={<RequireAuthh> <ProtectedComponent></ProtectedComponent></RequireAuth>}/>*/}
      {/*<Route path='*' element={<NotFoundPage />}/>*/}
    </Route>
  )
);

export default router;
