

import           AOC2017
import           System.Environment
import           System.FilePath
import           Text.Printf
import           Text.Read
import qualified Data.IntMap        as IM

tests :: IM.IntMap Challenge
tests = IM.fromList [(1, day01)]

main :: IO ()
main = do
    a:_ <- getArgs
    let Just d = readMaybe a
        Just t = IM.lookup d tests
    putStrLn . t =<< readFile ("data" </> printf "%02d" d <.> "txt")

