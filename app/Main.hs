{-# LANGUAGE OverloadedStrings, DeriveGeneric, PatternSynonyms #-}

module Main where

import qualified LastSum.Lib.API.LastFM as LastFM
import LastSum.Lib.API.LastFM.Parsing (ArtistRecord, pattern ArtistRecord, parseArtistsResponse)
import qualified LastSum.Settings as Settings

import Network.HTTP.Conduit (simpleHttp)
import qualified Data.ByteString.Lazy as B
import Data.Either.Utils
import Data.String.Utils (replace)
import Control.Monad.Except
import Control.Monad.Trans.Maybe
import Text.Read (readMaybe)


makeTopArtistsReport :: String -> [ArtistRecord] -> String
makeTopArtistsReport banner topArtists = banner ++ body
    where
        body = foldr1 (\a b -> a++", "++b) $ fmap asString topArtists
        asString (ArtistRecord name count) = name ++ " (" ++ (show count) ++ ")"

artistsURLDataToReport :: String -> B.ByteString -> Maybe String
artistsURLDataToReport banner j = do
    ar <- parseArtistsResponse j
    return $ makeTopArtistsReport banner ar


--  an attempt to reimplement main with maybeT

data ReportSpec = TopArtistsReport String LastFM.RetroPeriod Int

specFromSettings :: Settings.Settings -> ReportSpec
specFromSettings settings = TopArtistsReport (Settings.lastUsername settings) (Settings.reportPeriod settings) (Settings.reportLimit settings)

specToRequest :: ReportSpec -> LastFM.Request
specToRequest (TopArtistsReport u p l) = LastFM.UserTopArtists u p l

specToBanner :: ReportSpec -> String
specToBanner (TopArtistsReport u p l) = "Top last.fm artists of " ++ LastFM.descriptiveName p ++ ": "

getReport :: Settings.Settings -> ReportSpec -> MaybeT IO String
getReport settings spec = do
    --config <- MaybeT $ Config.getConfig
    let url = LastFM.requestURL (Settings.lastAPIKey settings) $ specToRequest spec
    j <- liftIO $ simpleHttp url
    MaybeT $ return $ artistsURLDataToReport (specToBanner spec) j

-- 

main :: IO ()
main = do
    s <- Settings.getSettings
    case s of 
        Left err -> putStrLn err
        Right settings -> do
            r <- runMaybeT $ getReport settings (specFromSettings settings)
            putStrLn $ maybe "Nothing to report" id r
