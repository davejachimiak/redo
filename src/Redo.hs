import Database.Redis
import System.Environment
import qualified Data.ByteString.Char8 as B
import Data.ByteString ( ByteString )
import Control.Applicative

data Command = Add Tasks deriving Show

type Tasks = [String]
type Args = [String]

main = do
    args <- getArgs
    response <- run args
    B.putStrLn response

run :: Args -> IO ByteString
run ("add":xs) = add xs
run ("list":_) = list

add :: [String] -> IO ByteString
add tasks = do
    result <- withRedis $ rpush namespace $ map B.pack tasks
    case result of
        Right _ -> return $ B.pack "Tasks added"
        Left reply -> return $ B.pack $ show reply

list :: IO ByteString
list = do
    let numericizeTask n t = B.pack (show n) `B.append` B.pack " -- " `B.append` t
    result <- withRedis $ lrange namespace 0 (-1)
    case result of
        Right ts -> return $ B.unlines $ zipWith numericizeTask [1..] ts
        Left reply -> return $ B.pack $ show reply

withRedis :: Redis a -> IO a
withRedis action = do
    conn <- connect defaultConnectInfo
    runRedis conn action

namespace :: ByteString
namespace = B.pack "Redo:tasks"
