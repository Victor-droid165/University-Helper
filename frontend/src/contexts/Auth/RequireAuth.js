import { Navigate, useLocation } from "react-router-dom";
import { useAuth } from "../../hooks/useAuth"
import { Outlet } from "react-router-dom"

export const RequireAuth = ({ allowedRoles }) => {
    const auth = useAuth();
    const location = useLocation();
    const isAuthenticated = auth.isAuthenticated();
    const isAuthorized = isAuthenticated && allowedRoles?.includes(auth?.user?.type)

    return isAuthorized 
            ? <Outlet />
            : isAuthenticated 
            ? <Navigate to='/home' state={{ path: location.pathname }} replace />
            : <Navigate to='/login' state={{ path: location.pathname }} replace />
}