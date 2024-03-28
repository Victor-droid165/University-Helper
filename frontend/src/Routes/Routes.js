import React from 'react';
import { Route, createBrowserRouter, createRoutesFromElements } from 'react-router-dom';
import AppPageLayout from '../layouts/AppPageLayout/AppPageLayout';
import HomePage from '../pages/home';
import RegisterPage, { registerAction } from '../pages/register';
import LoginPage from '../pages/login';
import AdminPage from '../pages/admin';
import NoteCreationPage from '../pages/notes/creation';
import { loginAction } from '../components/UserLoginForm/UserLoginForm';
import ProtectedRoutes from './ProtectedRoutes'
import LogoutPage from '../pages/logout/LogoutPage';
import ListNotes from '../pages/notes/list';
import EditeNote from '../pages/notes/edition';

const router = createBrowserRouter(
  createRoutesFromElements(
    <Route path='/' element={<AppPageLayout />}>
      <Route index element={<HomePage />} />
      <Route path="home" element={<HomePage />} />
      <Route path="register" element={<RegisterPage />} action={registerAction} />
      <Route path="login" element={<LoginPage />} action={loginAction} />
      <Route path="logout" element={<LogoutPage />} />

      <Route element={<ProtectedRoutes allowedRoles={["Admin"]} />}>
        <Route path="admin" element={<AdminPage />} />
      </Route>

      <Route element={<ProtectedRoutes allowedRoles={["Admin", "Professor", "Student"]} />}>
        <Route path="note-creation" element={<NoteCreationPage />} />
        <Route path="note-list" element={<ListNotes />} />
        <Route path="note-edition" element={<EditeNote />} />
      </Route>

      {/* <Route path="admin" element={<Album />}/> */}

      {/*<Route path='*' element={<NotFoundPage />}/>*/}
    </Route>
  )
);

export default router;
