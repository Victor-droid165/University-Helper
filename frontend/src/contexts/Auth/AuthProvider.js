import { useEffect, useState } from "react";
import { AuthContext } from "./AuthContext";

export const AuthProvider = ({ children }) => {
    const [user, setUser] = useState();

    const login = (user) => {
        localStorage.setItem("loggedUser", JSON.stringify(user));
        setUser(user);
    }

    const logout = () => {
        localStorage.removeItem("loggedUser");
        setUser(null);
    }

    const isAuthenticated = () => {
        //return Boolean(user);
        return Boolean(localStorage.getItem("loggedUser"))
    }

    useEffect(() => {
        const loggedUser = localStorage.getItem("loggedUser")
        if(loggedUser) setUser(loggedUser)
    }, [])

    return <AuthContext.Provider value={{ user, login, logout, isAuthenticated }}> {children} </AuthContext.Provider>
}