{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Main where

import qualified LastSum.Lib.API.LastFM as LastFM
import qualified LastSum.Config as Config

import Network.HTTP.Conduit (simpleHttp)
import qualified Data.ByteString.Lazy as B
import qualified Data.ConfigFile as CF
import Data.Either.Utils
import Data.String.Utils (replace)
import Control.Monad.Error
import Control.Monad.Trans.Maybe
import System.Directory
import Text.Read (readMaybe) -- readMaybe
import Data.Aeson
import Data.Aeson.Types


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



urlDataToReport :: B.ByteString -> Maybe String
urlDataToReport j = do
	d <- decode j
	ar <- parseMaybe topArtistsResponse d
	return $ makeReport ar


--  an attempt to reimplement main with maybeT

getReport :: MaybeT IO String
getReport = do
	config <- MaybeT $ Config.getConfig
	let url = LastFM.requestURL (config.lastAPIKey config) $ LastFM.UserTopArtists (config.lastUsername config) LastFM.P7Day 5
	j <- liftIO $ simpleHttp url
	return $ urlDataToReport j

main :: IO ()
main = do
	r <- runMaybeT getReport
	putStrLn $ maybe "Nothing to report" id r