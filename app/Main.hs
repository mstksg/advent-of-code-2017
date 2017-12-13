{-# LANGUAGE ApplicativeDo   #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns    #-}

-- import           Control.Monad.IO.Class
-- import           Control.Monad.Trans.Except
-- import           Data.Foldable
-- import           Data.List
-- import           Network.Curl
-- import           System.FilePath
import           AOC2017
import           Control.Applicative
import           Control.DeepSeq
import           Control.Exception
import           Control.Monad
import           Criterion
import           Data.Char
import           Data.Finite
import           Data.Maybe
import           Data.Semigroup
import           GHC.Generics                  (Generic)
import           Options.Applicative
import           System.IO.Error
import           Text.Printf
import           Text.Read
import qualified Data.Aeson                    as A
import qualified Data.ByteString               as BS
import qualified Data.IntMap                   as IM
import qualified Data.Map                      as M
import qualified Data.Yaml                     as Y
import qualified System.Console.ANSI           as ANSI

data TestSpec = TSAll
              | TSDayAll  { _tsDay  :: Finite 25 }
              | TSDayPart { _tsDay  :: Finite 25
                          , _tsPart :: Char
                          }
  deriving Show

data Opts = O { _oTestSpec :: TestSpec
              , _oTests    :: Bool
              , _oBench    :: Bool
              , _oLock     :: Bool
              , _oConfig   :: Maybe FilePath
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
      Right cs -> flip (runAll _cfgSession _oLock) cs $ \c CD{..} -> do
        case _cdInp of
          Left err | not _oTests || _oBench -> do
            putStrLn "[ERROR]"
            mapM_ (putStrLn . ("  " ++)) err
          _ ->
            return ()
        when _oTests $ do
          testRes <- mapMaybe fst <$> mapM (uncurry (testCase True c)) _cdTests
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
    -> Bool
    -> (Challenge -> ChallengeData -> IO ())
    -> IM.IntMap (M.Map Char Challenge)
    -> IO ()
runAll sess lock f = fmap void          $
                     IM.traverseWithKey $ \d ->
                     M.traverseWithKey  $ \p c -> do
    let CP{..} = challengePaths d p
    printf ">> Day %02d%c\n" d p
    when lock $ do
      CD{..} <- challengeData sess d p
      forM_ _cdInp $ \inp ->
        writeFile _cpAnswer =<< evaluate (force (c inp))
    f c =<< challengeData sess d p

testCase
    :: Bool
    -> Challenge
    -> String
    -> Maybe String
    -> IO (Maybe Bool, String)
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
    return (status, res)
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
    l <- switch $ long "lock"
               <> short 'l'
               <> help "Lock in results as \"correct\" answers"
    c <- optional $ strOption
        ( long "config"
       <> short 'c'
       <> metavar "PATH"
       <> help "Path to configuration file (default: aoc2017-conf.yaml)"
        )
    pure $ let ts = case d of
                      Just d' -> case p of
                        Just p' -> TSDayPart d' p'
                        Nothing -> TSDayAll  d'
                      Nothing -> TSAll
           in  O ts t b l c
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

configJSON :: A.Options
configJSON = A.defaultOptions
    { A.fieldLabelModifier = A.camelTo2 '-' . drop 4 }

instance A.ToJSON Config where
    toJSON     = A.genericToJSON configJSON
    toEncoding = A.genericToEncoding configJSON
instance A.FromJSON Config where
    parseJSON  = A.genericParseJSON configJSON

