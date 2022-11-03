import System.Environment
import RQLQTokens
import RQLQGrammar
import RQLTokens
import RQLGrammar

main :: IO ()
main = do
          x <- getArgs
          y <- readFile (head x)
          --print $ alexScanTokens y
          print $ (parseQuery . RQLQTokens.alexScanTokens) y