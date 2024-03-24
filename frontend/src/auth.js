import { createContext, useContext, useState } from "react";

const AuthContext = createContext(null)

export const AuthProvider = ({ children }) => {
    const [user, setUser] = useState();

    const login = (user) => {
        setUser(user);
    }

    const logout = () => {
        setUser(null);
    }

    const isAuthenticated = () => {
        return Boolean(user);
    }

    return <AuthContext.Provider value={{ user, login, logout, isAuthenticated }}> {children} </AuthContext.Provider>
}

export const useAuth = () => {
    return useContext(AuthContext);
}