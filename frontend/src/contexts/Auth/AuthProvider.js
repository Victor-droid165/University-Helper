import { useState } from "react";
import { AuthContext } from "./AuthContext";

export const AuthProvider = ({ children }) => {
    const [user, setUser] = useState(JSON.parse(localStorage.getItem("loggedUser")));

    const login = (user) => {
        localStorage.setItem("loggedUser", JSON.stringify(user));
        setUser(user);
    }

    const logout = () => {
        localStorage.removeItem("loggedUser");
        setUser(null);
    }

    const isAuthenticated = () => {
        return Boolean(user);
    }

    const isAuthorized = ({ authorizedRoles }) => {
        return authorizedRoles?.includes(user?.type);
    }

    return <AuthContext.Provider value={{ user, login, logout, isAuthenticated, isAuthorized }}> {children} </AuthContext.Provider>
}