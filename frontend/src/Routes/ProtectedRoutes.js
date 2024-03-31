import React from "react";
import { RequireAuth } from "../contexts/Auth/RequireAuth";

const ProtectedRoutes = ({ allowedRoles }) => {
    return <RequireAuth allowedRoles={allowedRoles} />
}

export default ProtectedRoutes;