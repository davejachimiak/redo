import Database.Redis
import System.Environment
import Data.ByteString.Char8 ( pack, unpack )
import Data.ByteString.Internal

main = do
    (command:arguments) <- getArgs

    execute command arguments

execute :: String -> [String] -> IO ()
execute "add" (task:_) = add task
execute "list" _       = list

list :: IO ()
list = do
  result <- withRedis $ lrange (pack "redo") 0 (-1)

  listFeedback result

listFeedback :: Either Reply [Data.ByteString.Internal.ByteString] -> IO ()
listFeedback (Left reply)  = putStrLn $ "Error: " ++ show reply
listFeedback (Right tasks) = mapM_ (putStrLn . unpack) tasks

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
