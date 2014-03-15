import Database.Redis
import System.Environment
import qualified Data.ByteString.Char8 as B
import Data.ByteString ( ByteString )
import Control.Applicative
import Data.Monoid

type Tasks = [String]
type Args = [String]

newtype AddResult e r = AddResult (Either Reply Integer)
newtype ListResult e r = ListResult (Either Reply [ByteString])
newtype RemoveAllResult e r = RemoveAllResult (Either Reply Integer)
newtype RemoveSingleResult e r = RemoveSingleResult (Either Reply Integer)

class Result r where
    handleResult :: r -> ByteString

instance Result (AddResult e r) where
    handleResult (AddResult (Right _)) = B.pack "Tasks added"
    handleResult (AddResult (Left e)) = B.pack $ show e

instance Result (ListResult e r) where
    handleResult (ListResult (Right ts)) =
        B.unlines $ zipWith numericizeTask [1..] ts
    handleResult (ListResult (Left e)) = B.pack $ show e

instance Result (RemoveAllResult e r) where
    handleResult (RemoveAllResult (Right _)) = B.pack "All tasks removed"
    handleResult (RemoveAllResult (Left e)) = B.pack $ show e

instance Result (RemoveSingleResult e r) where
    handleResult (RemoveSingleResult (Right _)) = B.pack "Task removed"
    handleResult (RemoveSingleResult (Left e)) = B.pack $ show e

main = do
    args <- getArgs
    response <- run args
    B.putStrLn response

run :: Args -> IO ByteString
run ("add":xs) = add xs
run ("list":_) = list
run ("remove":"-a":_) = removeAll
run ("remove":"--all":_) = removeAll
run ("remove":"-s":n:_) = removeSingle n

add :: [String] -> IO ByteString
add tasks = do
    let command = rpush namespace $ map B.pack tasks
    handleResult . AddResult <$> withRedis command

list :: IO ByteString
list = do
    let command = lrange namespace 0 (-1)
    handleResult . ListResult <$> withRedis command

removeAll :: IO ByteString
removeAll = do
    let command = del [namespace]
    handleResult . RemoveAllResult <$> withRedis command

removeSingle :: String -> IO ByteString
removeSingle n = do
    let fetchCommand = lindex namespace $ (read n :: Integer) - 1

    fetchResult <- withRedis fetchCommand
    
    case fetchResult of
        Left e -> return $ B.pack "Error: " `mappend` packShow e
        Right (Just t) ->
            handleResult . RemoveSingleResult <$> withRedis (lrem namespace 1 t)
        Right Nothing -> return $ B.pack "Task not found"

numericizeTask :: Integer -> ByteString -> ByteString
numericizeTask n t = mconcat [packShow n, separator, t]

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
