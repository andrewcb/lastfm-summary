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
import qualified Options.Applicative as OPT
import Data.Semigroup ((<>))
import System.Directory
import Text.Read (readMaybe) -- readMaybe
import Data.Aeson
import Data.Aeson.Types

-- command line options

data CommandLineOptions = CommandLineOptions {
    optPeriod :: LastFM.TopArtistsPeriod,
    optLimit :: Int
} deriving Show

parsePeriod :: OPT.ReadM LastFM.TopArtistsPeriod
parsePeriod = OPT.eitherReader $ \s -> case LastFM.parseTopArtistsPeriod s of
    Just p -> Right p
    Nothing -> Left "Invalid period specified"

commandLineParser :: OPT.Parser CommandLineOptions
commandLineParser = CommandLineOptions
    <$> OPT.option parsePeriod  ( 
        OPT.long "period" 
        <> OPT.short 'p' 
        <> OPT.help "period of report"
        <> OPT.value LastFM.P7Day )
    <*> OPT.option OPT.auto (
        OPT.long "limit"
        <> OPT.short 'l'
        <> OPT.help "number of entries to list"
        <> OPT.metavar "N"
        <> OPT.value 5
        <> OPT.showDefault )

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

makeReport :: String -> [ArtistRecord] -> String
makeReport banner topArtists = banner ++ body
	where
		body = foldr1 (\a b -> a++", "++b) $ fmap asString topArtists
		asString (ArtistRecord name count) = name ++ " (" ++ (show count) ++ ")"



urlDataToReport :: B.ByteString -> Maybe String
urlDataToReport j = do
	d <- decode j
	ar <- parseMaybe topArtistsResponse d
	return $ makeReport "Top last.fm artists of the past week: " ar


--  an attempt to reimplement main with maybeT

data ReportSpec = TopArtistsReport LastFM.TopArtistsPeriod Int

getReport :: ReportSpec -> MaybeT IO String
getReport spec = do

	config <- MaybeT $ Config.getConfig
	let request = case spec of 
		TopArtistsReport period limit -> LastFM.UserTopArtists (Config.lastUsername config) period limit
	let url = LastFM.requestURL (Config.lastAPIKey config) request
	j <- liftIO $ simpleHttp url
	MaybeT $ return $ urlDataToReport j

-- 

main' :: CommandLineOptions -> IO ()
main' opts@(CommandLineOptions period limit) = do
    --putStrLn $ show opts
    let reportSpec = TopArtistsReport period limit
    r <- runMaybeT $ getReport reportSpec
    putStrLn $ maybe "Nothing to report" id r

main :: IO ()
main = main' =<< OPT.execParser opts
    where
        opts = OPT.info (commandLineParser OPT.<**> OPT.helper)
            ( OPT.fullDesc 
                <> OPT.progDesc "produce a report from last.fm user listening data"
                <> OPT.header "lastfm-summary" )