import React from "react";
import { RequireAuth } from "../contexts/Auth/RequireAuth";

const ProtectedRoutes = ({ children }) => {
    return <RequireAuth children={children} />
}

export default ProtectedRoutes;