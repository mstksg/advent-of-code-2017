{-# LANGUAGE ApplicativeDo   #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns    #-}

import           AOC2017
import           Control.Applicative
import           Control.DeepSeq
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except
import           Criterion
import           Data.Char
import           Data.Finite
import           Data.Foldable
import           Data.List
import           Data.Maybe
import           Data.Semigroup
import           GHC.Generics                (Generic)
import           Network.Curl
import           Options.Applicative
import           System.FilePath
import           System.IO.Error
import           Text.Printf
import           Text.Read
import qualified Data.Aeson                  as A
import qualified Data.ByteString             as BS
import qualified Data.IntMap                 as IM
import qualified Data.Map                    as M
import qualified Data.Text                   as T
import qualified Data.Yaml                   as Y
import qualified System.Console.ANSI         as ANSI

data TestSpec = TSAll
              | TSDayAll  { _tsDay  :: Finite 25 }
              | TSDayPart { _tsDay  :: Finite 25
                          , _tsPart :: Char
                          }
  deriving Show

data Opts = O { _oTestSpec :: TestSpec
              , _oTests    :: Bool
              , _oBench    :: Bool
              , _oConfig   :: Maybe FilePath
              }

data ChallengeData = CD { _cdInp   :: !(Either [String] String)
                        , _cdAns   :: !(Maybe String)
                        , _cdTests :: ![(String, Maybe String)]
                        }

data Config = Cfg { _cfgSession :: Maybe String }
  deriving (Generic)

main :: IO ()
main = do
    O{..} <- execParser $ info (parseOpts <**> helper)
                ( fullDesc
               <> header "aoc2017 - Advent of Code 2017 challenge runner"
               <> progDesc "Run challenges from Advent of Code 2017"
                )
    Cfg{..} <- configFile $ fromMaybe "aoc2017-conf.yaml" _oConfig
    let toRun = case _oTestSpec of
          TSAll -> Right challengeMap
          TSDayAll (succ.fromIntegral->d) ->
            case IM.lookup d challengeMap of
              Nothing -> Left  $ printf "Day not yet available: %d" d
              Just cs -> Right $ IM.singleton d cs
          TSDayPart (succ.fromIntegral->d) p -> do
            ps <- maybe (Left $ printf "Day not yet available: %d" d) Right $
                    IM.lookup d challengeMap
            c  <- maybe (Left $ printf "Part not found: %c" p) Right $
                    M.lookup p ps
            return $ IM.singleton d (M.singleton p c)
    case toRun of
      Left e   -> putStrLn e
      Right cs -> flip (runAll _cfgSession) cs $ \c CD{..} -> do
        case _cdInp of
          Left err | not _oTests || _oBench -> do
            putStrLn "[ERROR]"
            mapM_ (putStrLn . ("  " ++)) err
          _ ->
            return ()
        when _oTests $ do
          testRes <- catMaybes <$> mapM (uncurry (testCase True c)) _cdTests
          unless (null testRes) $ do
            let (mark, color)
                    | and testRes = ('✓', ANSI.Green)
                    | otherwise   = ('✗', ANSI.Red  )
            ANSI.setSGR [ ANSI.SetColor ANSI.Foreground ANSI.Vivid color ]
            printf "[%c] Passed %d out of %d test(s)\n"
                mark
                (length (filter id testRes))
                (length testRes)
            ANSI.setSGR [ ANSI.Reset ]
        when (_oTests || not _oBench) . forM_ _cdInp $ \inp ->
          testCase False c inp _cdAns
        when _oBench . forM_ _cdInp $ \inp ->
          benchmark (nf c inp)

runAll
    :: Maybe String
    -> (Challenge -> ChallengeData -> IO ())
    -> IM.IntMap (M.Map Char Challenge)
    -> IO ()
runAll sess f = fmap void          $
                IM.traverseWithKey $ \d ->
                M.traverseWithKey  $ \p c -> do
    printf ">> Day %02d%c\n" d p
    f c =<< getData d p
  where
    getData :: Int -> Char -> IO ChallengeData
    getData d p = do
        inp   <- runExceptT . asum $
          [ ExceptT $ maybe (Left [fileErr]) Right <$> readFileMaybe inpFn
          , fetchInput
          ]
        ans   <- readFileMaybe ansFn
        ts    <- foldMap (parseTests . lines) <$> readFileMaybe testsFn
        return $ CD inp ans ts
      where
        fileErr :: String
        fileErr = printf "Input file not found at %s" inpFn
        fetchInput :: ExceptT [String] IO String
        fetchInput = do
          s <- maybe (throwE ["Session key needed to fetch input"]) return $
            sess
          (cc, r) <- liftIO . withCurlDo . curlGetString dataUrl $
              CurlCookie (printf "session=%s" s) : method_GET
          case cc of
            CurlOK -> return ()
            _      -> throwE [ "Error contacting advent of code server to fetch input"
                             , "Possible invalid session key"
                             , printf "Url: %s" dataUrl
                             , printf "Server response: %s" r
                             ]
          liftIO $ writeFile inpFn r
          return r
        dataUrl = printf "http://adventofcode.com/2017/day/%d/input" d
        inpFn   = "data"     </> printf "%02d" d <.> "txt"
        ansFn   = "data/ans" </> printf "%02d%c" d p <.> "txt"
        testsFn = "test-data" </> printf "%02d%c" d p <.> "txt"
    parseTests :: [String] -> [(String, Maybe String)]
    parseTests xs = case break (">>> " `isPrefixOf`) xs of
      (strip.unlines->inp,[])
        | null inp  -> []
        | otherwise -> [(inp, Nothing)]
      (strip.unlines->inp,(strip.drop 4->ans):rest)
        | null inp  -> parseTests rest
        | otherwise ->
            let ans' = ans <$ guard (not (null ans))
            in  (inp, ans') : parseTests rest
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
    forM_ showAns $ \a -> do
      ANSI.setSGR [ ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Red ]
      printf "(Expected: %s)\n" a
      ANSI.setSGR [ ANSI.Reset ]
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
    c <- optional $ strOption
        ( long "config"
       <> short 'c'
       <> metavar "PATH"
       <> help "Path to configuration file (default: aoc2017-conf.yaml)"
        )
    pure $ case d of
      Just d' -> case p of
        Just p' -> O (TSDayPart d' p') t b c
        Nothing -> O (TSDayAll  d') t b c
      Nothing -> O TSAll t b c
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

configFile :: FilePath -> IO Config
configFile fp = do
    cfgInp <- tryJust (guard . isDoesNotExistError)
            $ BS.readFile fp
    case cfgInp of
      Left () -> do
        Y.encodeFile fp emptyCfg
        return emptyCfg
      Right b -> do
        case Y.decodeEither b of
          Left e -> do
            printf "Configuration file at %s could not be parsed:\n" fp
            print e
            return emptyCfg
          Right cfg -> return cfg
  where
    emptyCfg = Cfg Nothing

strip :: String -> String
strip = T.unpack . T.strip . T.pack

configJSON :: A.Options
configJSON = A.defaultOptions
    { A.fieldLabelModifier = A.camelTo2 '-' . drop 4 }

instance A.ToJSON Config where
    toJSON     = A.genericToJSON configJSON
    toEncoding = A.genericToEncoding configJSON
instance A.FromJSON Config where
    parseJSON  = A.genericParseJSON configJSON

