import Database.Redis
import System.Environment
import Data.ByteString.Char8

main = do
    (_:task:_) <- getArgs
    conn <- connect defaultConnectInfo

    add conn task

add :: Connection -> String -> IO (Either Reply Integer)
add conn task = runRedis conn $ do
    rpush (pack "redo") [(pack task)]
