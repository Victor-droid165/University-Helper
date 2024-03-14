import System.Process
import System.Exit
import Control.Concurrent (threadDelay)
import System.IO (hPutStrLn, stderr)
import System.Environment (lookupEnv)

main :: IO ()
main = do
    dockerInstalled <- isDockerInstalled
    if not dockerInstalled
        then do
            hPutStrLn stderr "Erro: Docker nao esta instalado ou nao se encontra na variavel de ambiente PATH."
            exitFailure
    else return ()

    callCommand "docker-compose up -d"

    waitForPostgres

isDockerInstalled :: IO Bool
isDockerInstalled = do
    mbDockerPath <- lookupEnv "DOCKER_PATH"
    checkDockerInstallation mbDockerPath

checkDockerInstallation :: Maybe String -> IO Bool
checkDockerInstallation (Just _) = return True
checkDockerInstallation Nothing = do
    exitCode <- callCommandWithExitCode "docker --version"
    return $ exitCode == ExitSuccess

waitForPostgres :: IO ()
waitForPostgres = do
    putStrLn "Aguardado inicializacao do PostgreSQL..."
    let maxRetries = 30
        delayMicroseconds = 5 * 10000
    waitForPostgres' maxRetries delayMicroseconds
  where
    waitForPostgres' retries delayMicroseconds
        | retries == 0 = putStrLn "Limite de tempo excedido durante a inicializacao do PostgreSQL."
        | otherwise = do
            exitCode <- callCommandWithExitCode "docker-compose exec -T postgres pg_isready"
            checkExitCode exitCode
      where
        checkExitCode ExitSuccess = putStrLn "PostgreSQL esta sendo executado."
        checkExitCode _ = do
            putStrLn "PostgreSQL ainda nao esta configurado. Tentando novamente..."
            threadDelay delayMicroseconds
            waitForPostgres' (retries - 1) delayMicroseconds


callCommandWithExitCode :: String -> IO ExitCode
callCommandWithExitCode cmd = do
    (_, _, _, processHandle) <- createProcess (shell cmd)
    waitForProcess processHandle
