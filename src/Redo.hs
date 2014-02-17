import Database.Redis
import System.Environment
import Data.ByteString.Char8

main = do
    (_:task:_) <- getArgs

    add task

add :: String -> IO (Either Reply Integer)
add task = withRedis $ rpush (pack "redo") [(pack task)]

withRedis :: Redis a -> IO a
withRedis f = do
    conn <- connect defaultConnectInfo
    runRedis conn f
