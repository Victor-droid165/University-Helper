import { Navigate, useLocation } from "react-router-dom";
import { useAuth } from "../../hooks/useAuth"

export const RequireAuth = ({ children }) => {
    const auth = useAuth();
    const location = useLocation();
    const isAuthenticated = auth.isAuthenticated();

    return isAuthenticated ? children : <Navigate to='/login' state={{ path: location.pathname }} />
}