import { Navigate, useLocation } from "react-router-dom";
import { useAuth } from "../../hooks/useAuth"
import { Outlet } from "react-router-dom"
import { useApi } from "../../hooks/useApi";
import { useEffect, useState } from "react";

export const RequireAuth = ({ allowedRoles }) => {
    const auth = useAuth();
    const api = useApi();
    const location = useLocation();
    const isAuthenticated = auth.isAuthenticated();
    const [isAuthorized, setIsAuthorized] = useState(null);

    useEffect(() => {
        const fetchUserField = async () => {
            try {
                const response = await api.getUserField({
                    unique_key_name: "email",
                    unique_key: auth.user.email,
                    attribute: "type"
                });

                const authorized = isAuthenticated && allowedRoles.includes(response);
                setIsAuthorized(authorized);
            } catch (error) {
                setIsAuthorized(false);
            }
        };

        fetchUserField();
    }, [api, auth?.user?.email, isAuthenticated, allowedRoles]);

    if (isAuthorized === null) {
        return null;
    }

    return isAuthorized
        ? <Outlet />
        : isAuthenticated
            ? <Navigate to='/' state={{ path: location.pathname }} replace />
            : <Navigate to='/login' state={{ path: location.pathname }} replace />
}