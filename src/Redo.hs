import Database.Redis
import System.Environment
import qualified Data.ByteString.Char8 as B
import Data.ByteString ( ByteString )
import Control.Applicative

main = do
    args <- map B.pack <$> getArgs
    result <- add $ tail args
    B.putStrLn result

add :: [ByteString] -> IO ByteString
add tasks = do
    result <- withRedis $ rpush namespace tasks
    case result of
        Right _ -> return $ B.pack "Tasks added"
        Left reply -> return $ B.pack $ show reply

withRedis :: Redis a -> IO a
withRedis action = do
    conn <- connect defaultConnectInfo
    runRedis conn action

namespace :: ByteString
namespace = B.pack "Redo:tasks"
