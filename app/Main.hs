

import           AOC2017
import           System.Environment
import           System.FilePath
import           Text.Printf
import           Text.Read
import qualified Data.IntMap        as IM
import qualified Data.Map           as M

tests :: IM.IntMap (M.Map String Challenge)
tests = IM.fromList [(1, M.fromList [("a", day01a)
                                    ,("b", day01b)])
                    ,(2, M.fromList [("a", day02a)
                                    ,("b", day02b)])
                    ,(3, M.fromList [("a", day03a)
                                    ,("b", day03b)])
                    ]

main :: IO ()
main = do
    a:p:_ <- getArgs
    let Just d = readMaybe a
        Just t = M.lookup p =<< IM.lookup d tests
    putStrLn . t =<< readFile ("data" </> printf "%02d" d <.> "txt")

