import Database.Redis
import System.Environment
import Data.ByteString.Char8 ( pack, unpack )
import Data.ByteString.Internal
import Data.List

data Arg = FlagArg String | NumberArg String deriving Show

main = do
    arguments <- getArgs

    execute arguments

execute :: [String] -> IO ()
execute ("add":task:_) = add task
execute ("list":_) = list
execute ("remove":arg:_) = handleRemoveArg $ transformRemoveArg arg
execute (command:_) = putStrLn $ "Unknown Command: " ++ command

transformRemoveArg :: String -> Arg
transformRemoveArg arg
    | "-" `isPrefixOf` arg = FlagArg arg
    | otherwise = NumberArg arg

handleRemoveArg :: Arg -> IO ()
handleRemoveArg (NumberArg numberString) = remove $ (read numberString :: Integer) - 1
handleRemoveArg (FlagArg "-a") = removeAll
handleRemoveArg (FlagArg _) = putStrLn "Unknown flag"

removeAll :: IO ()
removeAll = do
    result <- withRedis $ del [namespace]
    putStrLn $ handleRemoveAll result

handleRemoveAll :: Either Reply Integer -> String
handleRemoveAll (Left reply) = "Error: " ++ show reply
handleRemoveAll (Right _) = "All tasks removed"

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
