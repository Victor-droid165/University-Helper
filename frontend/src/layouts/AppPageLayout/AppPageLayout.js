import NavBar from "../../components/NavBar/NavBar";
import styles from './AppPageLayout.module.css'

const AppPageLayout = ({ children, navData }) => {
    const defaultData = [
        { name: "Home", link: "/" },
        { name: "Login", link: "/login" },
        { name: "Register", link: "/register" },
        { name: "Help", link: "https://copilot.microsoft.com/" },
      ];
    return (
        <>
            <header>
                <NavBar data={navData || defaultData} />
            </header>
            <main className={styles.mainContent}>
                {children}
            </main>
        </>
    )
}

export default AppPageLayout