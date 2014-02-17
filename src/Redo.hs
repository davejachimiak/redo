import Database.Redis
import System.Environment
import Data.ByteString.Char8

main = do
    (_:task:_) <- getArgs

    conn <- connect defaultConnectInfo

    runRedis conn $ do
        rpush (pack "redo") [(pack task)]
