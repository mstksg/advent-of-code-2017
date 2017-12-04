{-# LANGUAGE ApplicativeDo   #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns    #-}

import           AOC2017
import           Control.Applicative
import           Control.DeepSeq
import           Control.Exception
import           Control.Monad
import           Criterion
import           Data.Char
import           Data.Finite
import           Data.Semigroup
import           Options.Applicative
import           System.FilePath
import           Text.Printf
import           Text.Read
import qualified Data.IntMap         as IM
import qualified Data.Map            as M

data TestSpec = TSAll
              | TSDayAll  { _tsDay  :: Finite 25 }
              | TSDayPart { _tsDay  :: Finite 25
                          , _tsPart :: Char
                          }
  deriving Show

data Opts = O { _oTestSpec :: TestSpec
              , _oBench    :: Bool
              }

tests :: IM.IntMap (M.Map Char Challenge)
tests = IM.fromList [(1, M.fromList [('a', day01a)
                                    ,('b', day01b)])
                    ,(2, M.fromList [('a', day02a)
                                    ,('b', day02b)])
                    ,(3, M.fromList [('a', day03a)
                                    ,('b', day03b)])
                    ,(4, M.fromList [('a', day04a)
                                    ,('b', day04b)])
                    ]

main :: IO ()
main = do
    O{..} <- execParser $ info (parseOpts <**> helper)
                ( fullDesc
               <> header "aoc2017 - Advent of Code 2017 challenge runner"
               <> progDesc "Run challenges from Advent of Code 2017"
                )
    let toRun = case _oTestSpec of
          TSAll -> Right tests
          TSDayAll (succ.fromIntegral->d) ->
            case IM.lookup d tests of
              Nothing -> Left  $ printf "Day not yet available: %d" d
              Just cs -> Right $ IM.singleton d cs
          TSDayPart (succ.fromIntegral->d) p -> do
            ps <- maybe (Left $ printf "Day not yet available: %d" d) Right $
                    IM.lookup d tests
            c  <- maybe (Left $ printf "Part not found: %c" p) Right $
                    M.lookup p ps
            return $ IM.singleton d (M.singleton p c)
    case toRun of
      Left e  -> putStrLn e
      Right cs
        | _oBench   -> flip runAll cs $ \c x -> benchmark (nf c x)
        | otherwise -> flip runAll cs $ \c x -> putStrLn (c x)

runAll :: (Challenge -> String -> IO ()) -> IM.IntMap (M.Map Char Challenge) -> IO ()
runAll f = void . IM.traverseWithKey runParts
  where
    runParts d = M.traverseWithKey $ \p c -> do
      printf ">> Day %02d%c\n" d p
      f c =<< evaluate . force
          =<< readFile ("data" </> printf "%02d" d <.> "txt")

parseOpts :: Parser Opts
parseOpts = do
    d <- argument pDay ( metavar "DAY"
                      <> help "Day of challenge (1 - 25), or \"all\""
                       )
    p <- optional $ argument pPart ( metavar "PART"
                                  <> help "Challenge part (a, b, c, etc.)"
                                   )
    b <- switch $ long "bench"
               <> short 'b'
               <> help "Run benchmarks"
    pure (($ b) $ case d of
            Just d' -> case p of
                         Just p' -> O $ TSDayPart d' p'
                         Nothing -> O $ TSDayAll  d'
            Nothing -> O TSAll
         )
  where
    pFin = eitherReader $ \s -> do
        n <- maybe (Left "Invalid day") Right $ readMaybe s
        maybe (Left "Day out of range") Right $ packFinite (n - 1)
    pDay = Nothing <$ maybeReader (guard . (== "all") . map toLower)
       <|>    Just <$> pFin
    pPart = eitherReader $ \case
        []  -> Left "No part"
        [p] | isAlpha p -> Right (toLower p)
            | otherwise -> Left "Invalid part (not an alphabet letter)"
        _   -> Left "Invalid part (not a single alphabet letter)"

