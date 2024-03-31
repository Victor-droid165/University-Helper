import { useEffect } from 'react';
import { useAuth } from '../../hooks/useAuth';
import { useNavigate } from 'react-router-dom';

const LogoutPage = () => {
    const auth = useAuth();
    const navigate = useNavigate();
    useEffect(() => {
        auth.logout();
        navigate('/', { replace: true });
    }, [auth, navigate]);

    return <div>Logging out...</div>;
};

export default LogoutPage;