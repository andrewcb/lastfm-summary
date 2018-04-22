{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Main where

import Lib

import Network.HTTP.Conduit (simpleHttp)
import qualified Data.ByteString.Lazy as B
import qualified Data.ConfigFile as CF
import Data.Either.Utils
import Control.Monad.Error
import System.Directory
import Text.Read -- readMaybe
import Data.Aeson
import Data.Aeson.Types

appName :: String
appName = "lastfm-summary"

topArtistsURL :: String -> String -> String
topArtistsURL username apiKey = "http://ws.audioscrobbler.com/2.0/?method=user.gettopartists&user=" ++ username ++ "&api_key=" ++ apiKey ++ "&format=json&period=7day&limit=5"

-- ConfigFile

configName :: IO String
configName = fmap (\d -> d ++ "/config") $ getAppUserDataDirectory appName

--readConfigFile :: MonadError CF.CPError m => IO (m CF.ConfigParser) 
--readConfigFile = do 
--	dataDir <- getAppUserDataDirectory appName
--	return $ CF.readfile CF.emptyCP $ dataDir ++ "/config"


-- TODO: make this return an Either
--getConfig :: Error e => IO (Either e (String, String))
getConfig :: IO (Maybe (String, String))
getConfig = do
	configFileName <- configName

	rv <- runErrorT $
		do
			cp <- join $ liftIO $ CF.readfile CF.emptyCP configFileName
			let x = cp
			--return $ (\a b -> (a,b)) <$> (CF.get x "DEFAULT" "username") <*> (CF.get x "DEFAULT" "api_key")
 			username <- CF.get x "DEFAULT" "username"
			apiKey <- CF.get x "DEFAULT" "api_key"
			return (username, apiKey)
	--return rv
	return $ case rv of
		Left _ -> Nothing
		Right (a, b) -> Just (a, b)

-- JSON parsing

data ArtistRecord = ArtistRecord {
	name :: String,
	playcount :: Int
} deriving (Show)

--data TopArtistsResponse = 

--topArtistsResponse :: Value -> Parser [ArtistRecord]
topArtistsResponse = withObject "topartists" $ \o -> do
	p <- o .: "topartists"
	artists <- p .: "artist"
	return artists :: Parser [ArtistRecord]
	--withObject "topartists" (\p -> p .: "artist") 
    --(o .: "topartists") .: "artist"
  -- o .: "topartists" .: "artist"

instance FromJSON ArtistRecord where
	--parseJSON = withObject "artistRecord" $ \o -> ArtistRecord <$> o .: "name" <*> (readMaybe (o .: "playcount")) :: Int
	parseJSON = withObject "artistRecord" $ \o -> do
		name <- o .: "name"
		countStr <- o .: "playcount"
		case readMaybe countStr :: Maybe Int of
			Just c -> return $ ArtistRecord name c
			Nothing -> fail "No well-formed playcount"
--

makeReport :: [ArtistRecord] -> String
makeReport topArtists = "Top last.fm artists of the past week: " ++ body
	where
		body = foldr1 (\a b -> a++", "++b) $ fmap asString topArtists
		asString (ArtistRecord name count) = name ++ " (" ++ (show count) ++ ")"

main :: IO ()

main = do
	rv <- getConfig
	case rv of
		Nothing -> return ()
		Just (username, apiKey) -> do
			let url = topArtistsURL username apiKey
			j <- simpleHttp url
			let val = decode j
			let m = val >>= parseMaybe topArtistsResponse
			--putStrLn $ show m
			putStrLn $ case m of 
				Just ar -> makeReport ar
				Nothing -> "Nothing to report"
			--putStrLn $ fmap makeReport m

