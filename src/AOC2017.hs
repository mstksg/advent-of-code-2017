{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns    #-}

module AOC2017 (
    module AOC
  , challengeMap
  , ChallengePaths(..), challengePaths
  , ChallengeData(..), challengeData
  , Config(..), configFile, defConfPath
  , session
  ) where

import           AOC2017.Day01 as AOC
import           AOC2017.Day02 as AOC
import           AOC2017.Day03 as AOC
import           AOC2017.Day04 as AOC
import           AOC2017.Day05 as AOC
import           AOC2017.Day06 as AOC
import           AOC2017.Day07 as AOC
import           AOC2017.Day08 as AOC
import           AOC2017.Day09 as AOC
import           AOC2017.Day10 as AOC
import           AOC2017.Day11 as AOC
import           AOC2017.Day12 as AOC
import           AOC2017.Day13 as AOC
import           AOC2017.Day14 as AOC

import           AOC2017.Types              as AOC
import           AOC2017.Util               as AOC
import           Control.DeepSeq
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except
import           Data.Foldable
import           Data.List
import           GHC.Generics               (Generic)
import           Network.Curl
import           System.FilePath
import           System.IO.Error
import           Text.Printf
import qualified Data.Aeson                 as A
import qualified Data.ByteString            as BS
import qualified Data.IntMap                as IM
import qualified Data.Map                   as M
import qualified Data.Yaml                  as Y

challengeMap :: IM.IntMap (M.Map Char Challenge)
challengeMap = IM.fromList
    [ (d, M.fromList [('a', ca),('b', cb)])
    | (d, (ca, cb)) <- challenges
    ]

challenges :: [(Int, (Challenge, Challenge))]
challenges = [ ( 1, (day01a, day01b))
             , ( 2, (day02a, day02b))
             , ( 3, (day03a, day03b))
             , ( 4, (day04a, day04b))
             , ( 5, (day05a, day05b))
             , ( 6, (day06a, day06b))
             , ( 7, (day07a, day07b))
             , ( 8, (day08a, day08b))
             , ( 9, (day09a, day09b))
             , (10, (day10a, day10b))
             , (11, (day11a, day11b))
             , (12, (day12a, day12b))
             , (13, (day13a, day13b))
             , (14, (day14a, day14b))
             ]

data ChallengePaths = CP { _cpDataUrl :: !FilePath
                         , _cpInput   :: !FilePath
                         , _cpAnswer  :: !FilePath
                         , _cpTests   :: !FilePath
                         }
  deriving Show

data ChallengeData = CD { _cdInp   :: !(Either [String] String)
                        , _cdAns   :: !(Maybe String)
                        , _cdTests :: ![(String, Maybe String)]
                        }

challengePaths
    :: Int
    -> Char
    -> ChallengePaths
challengePaths d p = CP
    { _cpDataUrl = printf "http://adventofcode.com/2017/day/%d/input" d
    , _cpInput   = "data"     </> printf "%02d" d <.> "txt"
    , _cpAnswer  = "data/ans" </> printf "%02d%c" d p <.> "txt"
    , _cpTests   = "test-data" </> printf "%02d%c" d p <.> "txt"
    }

challengeData
    :: Maybe String
    -> Int
    -> Char
    -> IO ChallengeData
challengeData sess d p = do
    inp   <- runExceptT . asum $
      [ ExceptT $ maybe (Left [fileErr]) Right <$> readFileMaybe _cpInput
      , fetchInput
      ]
    ans   <- readFileMaybe _cpAnswer
    ts    <- foldMap (parseTests . lines) <$> readFileMaybe _cpTests
    return $ CD inp ans ts
  where
    CP{..} = challengePaths d p
    fileErr = printf "Input file not found at %s" _cpInput
    readFileMaybe :: FilePath -> IO (Maybe String)
    readFileMaybe =
        (traverse (evaluate . force) . either (const Nothing) Just =<<)
       . tryJust (guard . isDoesNotExistError)
       . readFile
    fetchInput :: ExceptT [String] IO String
    fetchInput = do
      s <- maybe (throwE ["Session key needed to fetch input"]) return
        sess
      (cc, r) <- liftIO . withCurlDo . curlGetString _cpDataUrl $
          CurlCookie (printf "session=%s" s) : method_GET
      case cc of
        CurlOK -> return ()
        _      -> throwE [ "Error contacting advent of code server to fetch input"
                         , "Possible invalid session key"
                         , printf "Url: %s" _cpDataUrl
                         , printf "Server response: %s" r
                         ]
      liftIO $ writeFile _cpInput r
      return r
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

data Config = Cfg { _cfgSession :: Maybe String }
  deriving (Generic)

defConfPath :: FilePath
defConfPath = "aoc2017-conf.yaml"

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

session :: FilePath -> IO (Maybe String)
session = fmap _cfgSession . configFile

configJSON :: A.Options
configJSON = A.defaultOptions
    { A.fieldLabelModifier = A.camelTo2 '-' . drop 4 }

instance A.ToJSON Config where
    toJSON     = A.genericToJSON configJSON
    toEncoding = A.genericToEncoding configJSON
instance A.FromJSON Config where
    parseJSON  = A.genericParseJSON configJSON

