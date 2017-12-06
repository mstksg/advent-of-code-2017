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
import           Data.List
import           Data.Maybe
import           Data.Semigroup
import           Options.Applicative
import           System.FilePath
import           System.IO.Error
import           Text.Printf
import           Text.Read
import qualified Data.IntMap         as IM
import qualified Data.Map            as M
import qualified Data.Text           as T
import qualified System.Console.ANSI as ANSI

data TestSpec = TSAll
              | TSDayAll  { _tsDay  :: Finite 25 }
              | TSDayPart { _tsDay  :: Finite 25
                          , _tsPart :: Char
                          }
  deriving Show

data Opts = O { _oTestSpec :: TestSpec
              , _oTests    :: Bool
              , _oBench    :: Bool
              }

data ChallengeData = CD { _cdInp   :: !(Either FilePath String)
                        , _cdAns   :: !(Maybe String)
                        , _cdTests :: ![(String, Maybe String)]
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
                    ,(5, M.fromList [('a', day05a)
                                    ,('b', day05b)])
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
      Right cs -> flip runAll cs $ \c CD{..} -> do
        case _cdInp of
          Left fn | not _oTests || _oBench ->
            printf "[ERROR] Puzzle input file %s not found\n" fn
          _ ->
            return ()
        when _oTests $ do
          testRes <- catMaybes <$> mapM (uncurry (testCase True c)) _cdTests
          unless (null testRes) $ do
            let (mark, color)
                    | and testRes = ('✓', ANSI.Green)
                    | otherwise   = ('✗', ANSI.Red  )
            ANSI.setSGR [ ANSI.SetColor ANSI.Foreground ANSI.Vivid color ]
            printf "[%c] Passed %d out of %d tests\n"
                mark
                (length (filter id testRes))
                (length testRes)
            ANSI.setSGR [ ANSI.Reset ]
        when (_oTests || not _oBench) . forM_ _cdInp $ \inp ->
            testCase False c inp _cdAns
        when _oBench . forM_ _cdInp $ \inp ->
          benchmark (nf c inp)

runAll
    :: (Challenge -> ChallengeData -> IO ())
    -> IM.IntMap (M.Map Char Challenge)
    -> IO ()
runAll f = fmap void          $
           IM.traverseWithKey $ \d ->
           M.traverseWithKey  $ \p c -> do
    printf ">> Day %02d%c\n" d p
    f c =<< getData d p
  where
    getData :: Int -> Char -> IO ChallengeData
    getData d p = do
        inp   <- maybe (Left inpFn) Right <$> readFileMaybe inpFn
        ans   <- readFileMaybe ansFn
        ts    <- foldMap (parseTests . lines) <$> readFileMaybe testsFn
        return $ CD inp ans ts
      where
        inpFn   = "data"     </> printf "%02d" d <.> "txt"
        ansFn   = "data/ans" </> printf "%02d%c" d p <.> "txt"
        testsFn = "test-data" </> printf "%02d%c" d p <.> "txt"
    parseTests :: [String] -> [(String, Maybe String)]
    parseTests xs = case break (">>> " `isPrefixOf`) xs of
      (inp,[])
        | null inp  -> []
        | otherwise -> [(strip (unlines inp), Nothing)]
      (inp,(strip.drop 4->ans):rest) ->
        let inp' = strip (unlines inp)
            ans' = ans <$ guard (not (null ans))
        in  (inp', ans') : parseTests rest
    readFileMaybe :: FilePath -> IO (Maybe String)
    readFileMaybe =
        (traverse (evaluate . force) . either (const Nothing) Just =<<)
       . tryJust (guard . isDoesNotExistError)
       . readFile

testCase :: Bool -> Challenge -> String -> Maybe String -> IO (Maybe Bool)
testCase emph c inp ans = do
    ANSI.setSGR [ ANSI.SetColor ANSI.Foreground ANSI.Vivid color ]
    printf "[%c]" mark
    ANSI.setSGR [ ANSI.Reset ]
    if emph
      then printf " (%s)\n" res
      else printf " %s\n" res
    mapM_ (printf "(Expected: %s)\n") showAns
    return status
  where
    res = c inp
    (mark, showAns, status) = case ans of
      Just (strip->ex)
        | strip res == ex -> ('✓', Nothing, Just True )
        | otherwise       -> ('✗', Just ex, Just False)
      Nothing             -> ('?', Nothing, Nothing   )
    color = case status of
      Just True  -> ANSI.Green
      Just False -> ANSI.Red
      Nothing    -> ANSI.Blue

parseOpts :: Parser Opts
parseOpts = do
    d <- argument pDay ( metavar "DAY"
                      <> help "Day of challenge (1 - 25), or \"all\""
                       )
    p <- optional $ argument pPart ( metavar "PART"
                                  <> help "Challenge part (a, b, c, etc.)"
                                   )
    t <- switch $ long "tests"
               <> short 't'
               <> help "Run sample tests"
    b <- switch $ long "bench"
               <> short 'b'
               <> help "Run benchmarks"
    pure $ case d of
      Just d' -> case p of
        Just p' -> O (TSDayPart d' p') t b
        Nothing -> O (TSDayAll  d') t b
      Nothing -> O TSAll t b
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

strip :: String -> String
strip = T.unpack . T.strip . T.pack
