import Database.Redis
import System.Environment
import qualified Data.ByteString.Char8 as B
import Data.ByteString ( ByteString )
import Control.Applicative
import Data.Monoid

type Tasks = [String]
type Args = [String]

data Add e r = Add (Either e Integer)
data ListResult e r = ListResult (Either e [ByteString]) deriving Show

class Result r where
    handleResult :: r -> ByteString

instance (Show e) => Result (Add e r) where
    handleResult (Add (Right _)) = B.pack "Tasks added"
    handleResult (Add (Left e)) = B.pack (show e)

instance (Show e) => Result (ListResult e r) where
    handleResult (ListResult (Right ts)) = B.unlines $ zipWith numericizeTask [1..] ts
    handleResult (ListResult (Left e)) = B.pack (show e)

main = do
    args <- getArgs
    response <- run args
    B.putStrLn response

run :: Args -> IO ByteString
run ("add":xs) = add xs
run ("list":_) = list

add :: [String] -> IO ByteString
add tasks = handleResult <$> Add <$> withRedis (rpush namespace $ map B.pack tasks)

list :: IO ByteString
list = handleResult <$> ListResult <$> withRedis (lrange namespace 0 (-1))

numericizeTask :: Integer -> ByteString -> ByteString
numericizeTask n t = mconcat [B.pack (show n), B.pack " -- ", t]

withRedis :: Redis a -> IO a
withRedis action = do
    conn <- connect defaultConnectInfo
    runRedis conn action

namespace :: ByteString
namespace = B.pack "Redo:tasks"
