import Database.Redis
import System.Environment
import Data.ByteString.Char8 ( pack )

main = do
    (command:arguments) <- getArgs

    execute command arguments

execute :: String -> [String] -> IO ()
execute "add" (task:_) = add task

add :: String -> IO ()
add task = do
    result <- withRedis $ rpush (pack "redo") [(pack task)]

    putStrLn $ addFeedback task result

addFeedback :: String -> Either Reply Integer -> String
addFeedback _ (Left reply) = "Error: " ++ show reply
addFeedback task (Right _) = "Task added: " ++ task

withRedis :: Redis a -> IO a
withRedis f = do
    conn <- connect defaultConnectInfo
    runRedis conn f
