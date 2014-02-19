import Database.Redis
import System.Environment
import Data.ByteString.Char8 ( pack, unpack )
import Data.ByteString.Internal

main = do
    arguments <- getArgs

    if length arguments > 0
        then execute arguments
        else do
            putStrLn "Commands: redo list"
            putStrLn "          redo add \"task name\""
            putStrLn "          redo remove task_number"

execute :: [String] -> IO ()
execute ("add":task:_) = add task
execute ("list":_) = list
execute ("remove":numberString:_) = remove $ (read numberString :: Integer) - 1
execute (command:_) = putStrLn $ "Unknown Command: " ++ command

add :: String -> IO ()
add task = do
    result <- withRedis $ rpush namespace [(pack task)]

    putStrLn $ addOutput task result

addOutput :: String -> Either Reply Integer -> String
addOutput _ (Left reply) = "Error: " ++ show reply
addOutput task (Right _) = "Task added: " ++ task

list :: IO ()
list = do
    result <- withRedis $ lrange namespace 0 (-1)

    listOutput result

listOutput :: Either Reply [Data.ByteString.Internal.ByteString] -> IO ()
listOutput (Left reply)  = putStrLn $ "Error: " ++ show reply
listOutput (Right tasks) = do
    let numericizeTask n task = show n ++ " -- " ++ unpack task
        numberedTasks         = zipWith numericizeTask [1..] tasks

    mapM_ putStrLn numberedTasks

remove :: Integer -> IO ()
remove n = do
    let fetchTask n = lindex namespace n
        handleFetchTaskResult (Left reply) = putStrLn $ "Error: " ++ show reply
        handleFetchTaskResult (Right (Just task)) = do
            result <- withRedis $ lrem namespace 1 task
            removeOutput task result
        handleFetchTaskResult (Right Nothing) = taskNotFound
        taskNotFound = putStrLn "Task not found."

    if n >= 0
        then do
            taskResult <- withRedis $ fetchTask n
            handleFetchTaskResult taskResult
        else taskNotFound

removeOutput :: ByteString -> Either Reply Integer -> IO ()
removeOutput _ (Left reply) = putStrLn $ "Error: " ++ show reply
removeOutput task (Right _) = putStrLn $ "Task removed: " ++ unpack task

withRedis :: Redis a -> IO a
withRedis action = do
    conn <- connect defaultConnectInfo
    runRedis conn action

namespace :: Data.ByteString.Internal.ByteString
namespace = pack "Redo:tasks"
