{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Main where

import qualified LastSum.Lib.API.LastFM as LastFM
import qualified LastSum.Settings as Settings

import Network.HTTP.Conduit (simpleHttp)
import qualified Data.ByteString.Lazy as B
import qualified Data.ConfigFile as CF
import Data.Either.Utils
import Data.String.Utils (replace)
import Control.Monad.Except
import Control.Monad.Trans.Maybe
--import System.Directory
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

data ReportSpec = TopArtistsReport String LastFM.TopArtistsPeriod Int

specFromSettings :: Settings.Settings -> ReportSpec
specFromSettings settings = TopArtistsReport (Settings.lastUsername settings) (Settings.reportPeriod settings) (Settings.reportLimit settings)

specToRequest :: ReportSpec -> LastFM.Request
specToRequest (TopArtistsReport u p l) = LastFM.UserTopArtists u p l

getReport :: Settings.Settings -> ReportSpec -> MaybeT IO String
getReport settings spec = do
    --config <- MaybeT $ Config.getConfig
    let url = LastFM.requestURL (Settings.lastAPIKey settings) $ specToRequest spec
    j <- liftIO $ simpleHttp url
    MaybeT $ return $ urlDataToReport j

-- 

main :: IO ()
main = do
    s <- Settings.getSettings
    case s of 
        Left err -> putStrLn err
        Right settings -> do
            r <- runMaybeT $ getReport settings (specFromSettings settings)
            putStrLn $ maybe "Nothing to report" id r
