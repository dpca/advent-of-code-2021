import System.Environment
import Data.List

main :: IO ()
main = do
    args <- getArgs
    putStrLn "The args are: "
    mapM_ putStrLn args
