import Database.Redis
import System.Environment
import Data.ByteString.Char8 ( pack, unpack )
import Data.ByteString.Internal

main = do
    (command:arguments) <- getArgs

    execute command arguments

execute :: String -> [String] -> IO ()
execute "add" (task:_) = add task
execute "list" _ = list
execute "remove" (nString:_) = remove $ 1 - (read nString :: Integer)
execute command _ = putStrLn $ "Unknown Command: " ++ command

add :: String -> IO ()
add task = do
    result <- withRedis $ rpush namespace [(pack task)]

    putStrLn $ addFeedback task result

addFeedback :: String -> Either Reply Integer -> String
addFeedback _ (Left reply) = "Error: " ++ show reply
addFeedback task (Right _) = "Task added: " ++ task

list :: IO ()
list = do
    result <- withRedis $ lrange namespace 0 (-1)

    listFeedback result

listFeedback :: Either Reply [Data.ByteString.Internal.ByteString] -> IO ()
listFeedback (Left reply)  = putStrLn $ "Error: " ++ show reply
listFeedback (Right tasks) = do
    let numericizeTask = (\n task -> show n ++ " -- " ++ unpack task)
        numberedTasks  = zipWith numericizeTask [1..] tasks

    mapM_ putStrLn numberedTasks

remove :: Integer -> IO ()
remove n = do
    (Right result) <- withRedis $ do
        (Right (Just cool)) <- fetchTask n

        lrem namespace 1 cool

    putStrLn $ show result

fetchTask :: RedisCtx m f => Integer -> m (f (Maybe ByteString))
fetchTask n = lindex namespace n

withRedis :: Redis a -> IO a
withRedis f = do
    conn <- connect defaultConnectInfo
    runRedis conn f

namespace :: Data.ByteString.Internal.ByteString
namespace = pack "Redo:tasks"
