import { useAuth } from "../../hooks/useAuth";
import NavBar from "../../components/NavBar/NavBar";
import styles from './AppPageLayout.module.css'
import { Outlet } from "react-router-dom"

const AppPageLayout = ({ navData }) => {
    const auth = useAuth();


    let defaultData = [
        { name: "Home", link: "/" },
        { name: "Login", link: "/login" },
        { name: "Register", link: "/register" },
        { name: "Help", link: "https://copilot.microsoft.com/" },
    ];

    if (auth.isAuthenticated()) {
        defaultData = defaultData.filter(item => item.name !== "Login" && item.name !== "Register");
        defaultData.push({ name: "Logout", link: "/logout" })
    }

    return (
        <>
            <header>
                <NavBar data={navData || defaultData} />
            </header>
            <main className={styles.mainContent}>
                <Outlet />
            </main>
        </>
    )
}

export default AppPageLayout