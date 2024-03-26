import { useContext } from "react";
import { AuthContext } from "../contexts/Auth/AuthContext";

export const useAuth = () => {
    return useContext(AuthContext);
}