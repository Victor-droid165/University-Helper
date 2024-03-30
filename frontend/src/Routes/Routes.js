import React from 'react';
import { Route, createBrowserRouter, createRoutesFromElements } from 'react-router-dom';
import AppPageLayout from '../layouts/AppPageLayout/AppPageLayout';
import HomePage from '../pages/home';
import RegisterPage from '../pages/register';
import LoginPage from '../pages/login';
import AdminPage from '../pages/admin';
import NoteCreationPage from '../pages/notes/creation';
import ProtectedRoutes from './ProtectedRoutes'
import LogoutPage from '../pages/logout/LogoutPage';
import ListNotes from '../pages/notes/list';
import EditNote from '../pages/notes/edition';

const router = createBrowserRouter(
  createRoutesFromElements(
    <Route path='/' element={<AppPageLayout />}>
      <Route index element={<HomePage />} />
      <Route path="home" element={<HomePage />} />
      <Route path="register" element={<RegisterPage />} />
      <Route path="login" element={<LoginPage />} />
      <Route path="logout" element={<LogoutPage />} />

      <Route element={<ProtectedRoutes allowedRoles={["Admin"]} />}>
        <Route path="admin" element={<AdminPage />} />
      </Route>

      <Route element={<ProtectedRoutes allowedRoles={["Admin", "Professor", "Student"]} />}>
        <Route path="note-creation" element={<NoteCreationPage />} />
        <Route path="note-list" element={<ListNotes />} />
        <Route path="note-edition" element={<EditNote />} />
      </Route>

      {/* <Route path="admin" element={<Album />}/> */}

      {/*<Route path='*' element={<NotFoundPage />}/>*/}
    </Route>
  )
);

export default router;
