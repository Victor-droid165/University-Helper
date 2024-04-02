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
    const [isAuthorized, setIsAuthorized] = useState(null); // Initialize as null while waiting for response

    useEffect(() => {
        const fetchUserField = async () => {
            try {
                const response = await api.getUserField({
                    unique_key_name: "email",
                    unique_key: auth.user.email,
                    attribute: "type"
                });
                // Check if the user's role is allowed
                const authorized = isAuthenticated && allowedRoles.includes(response);
                setIsAuthorized(authorized);
            } catch (error) {
                console.error("Error fetching user field:", error);
                // Handle error: Redirect to login page or display an error message
                setIsAuthorized(false);
            }
        };

        // Call fetchUserField when component mounts
        fetchUserField();
    }, [api, auth?.user?.email, isAuthenticated, allowedRoles]);

    // During loading or while awaiting response, return null or a loading indicator
    if (isAuthorized === null) {
        return null;
    }
    
    return isAuthorized
        ? <Outlet />
        : isAuthenticated
            ? <Navigate to='/home' state={{ path: location.pathname }} replace />
            : <Navigate to='/login' state={{ path: location.pathname }} replace />
}