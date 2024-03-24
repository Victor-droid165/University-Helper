import { Navigate, useLocation } from "react-router-dom";
import { useAuth } from "./auth"

export const RequireAuth = ({ children }) => {
    const auth = useAuth();
    const location = useLocation();

    if (!auth.isAuthenticated()) {
        return <Navigate to='/login' state={{ path: location.pathname }} />
    }

    return children;
}