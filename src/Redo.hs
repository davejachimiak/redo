import Database.Redis
import System.Environment
import qualified Data.ByteString.Char8 as B
import Data.ByteString ( ByteString )
import Control.Applicative
import Data.Monoid

type Tasks = [String]
type Args = [String]

newtype AddResult = AddResult (Either Reply Integer)
newtype ListResult = ListResult (Either Reply [ByteString])
newtype RemoveAllResult = RemoveAllResult (Either Reply Integer)
newtype RemoveSingleResult = RemoveSingleResult (Either Reply Integer)

class Response r where
    handleResult :: r -> ByteString

instance Response AddResult where
    handleResult (AddResult (Right _)) = B.pack "Tasks added"
    handleResult (AddResult (Left e)) = errorResponse e

instance Response ListResult where
    handleResult (ListResult (Right ts)) =
        B.unlines $ zipWith numericizeTask [1..] ts
    handleResult (ListResult (Left e)) = errorResponse e

instance Response RemoveAllResult where
    handleResult (RemoveAllResult (Right _)) = B.pack "All tasks removed"
    handleResult (RemoveAllResult (Left e)) = errorResponse e

instance Response RemoveSingleResult where
    handleResult (RemoveSingleResult (Right _)) = B.pack "Task removed"
    handleResult (RemoveSingleResult (Left e)) = errorResponse e

main = do
    args <- getArgs
    response <- run args
    B.putStrLn response

run :: Args -> IO ByteString
run ("add":xs) = getResponse AddResult $ rpush namespace (map B.pack xs)
run ("list":_) = getResponse ListResult $ lrange namespace 0 (-1)
run ("remove":"-a":_) = removeAll
run ("remove":"--all":_) = removeAll
run ("remove":"-s":n:_) = removeSingle n
run ("remove":"--single":n:_) = removeSingle n

getResponse :: Response c => (a -> c) -> Redis a -> IO ByteString
getResponse wrapper = liftA (handleResult . wrapper) . withRedis

removeAll :: IO ByteString
removeAll = getResponse RemoveAllResult $ del [namespace]

removeSingle :: String -> IO ByteString
removeSingle n = do
    fetchResult <- withRedis $ lindex namespace $ (read n :: Integer) - 1
    
    case fetchResult of
        Left e -> return $ errorResponse e
        Right (Just t) -> getResponse RemoveSingleResult $ lrem namespace 1 t
        Right Nothing -> return $ B.pack "Task not found"

numericizeTask :: Integer -> ByteString -> ByteString
numericizeTask n t = mconcat [packShow n, separator, t]

errorResponse :: Reply -> ByteString
errorResponse = mappend (B.pack "Error: ") . packShow

packShow :: Show a => a -> ByteString
packShow = B.pack . show

separator :: ByteString
separator = B.pack " -- "

withRedis :: Redis a -> IO a
withRedis action = do
    conn <- connect defaultConnectInfo
    runRedis conn action

namespace :: ByteString
namespace = B.pack "Redo:tasks"
