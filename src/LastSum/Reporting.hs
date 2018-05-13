{-# LANGUAGE OverloadedStrings, PatternSynonyms #-}

module LastSum.Reporting
where

import qualified LastSum.Settings as Settings
import qualified LastSum.Lib.API.LastFM as LastFM
import LastSum.Lib.API.LastFM.Parsing (ArtistRecord, pattern ArtistRecord, parseArtistsResponse)
import Network.HTTP.Conduit (simpleHttp)
import qualified Data.ByteString.Lazy as B
import Data.Either.Utils
import Control.Monad.Except
import Control.Monad.Trans.Maybe


data ReportSpec = TopArtistsReport String LastFM.RetroPeriod Int

makeTopArtistsReport :: String -> [ArtistRecord] -> String
makeTopArtistsReport banner topArtists = banner ++ body
    where
        body = foldr1 (\a b -> a++", "++b) $ fmap asString topArtists
        asString (ArtistRecord name count) = name ++ " (" ++ (show count) ++ ")"

artistsURLDataToReport :: String -> B.ByteString -> Maybe String
artistsURLDataToReport banner j = do
    ar <- parseArtistsResponse j
    return $ makeTopArtistsReport banner ar



specToRequest :: ReportSpec -> LastFM.Request
specToRequest (TopArtistsReport u p l) = LastFM.UserTopArtists u p l

specToBanner :: ReportSpec -> String
specToBanner (TopArtistsReport u p l) = "Top last.fm artists of " ++ LastFM.descriptiveName p ++ ": "

getReport :: String -> ReportSpec -> MaybeT IO String
getReport apiKey spec = do
    --config <- MaybeT $ Config.getConfig
    let url = LastFM.requestURL apiKey $ specToRequest spec
    j <- liftIO $ simpleHttp url
    MaybeT $ return $ case spec of
        TopArtistsReport _ _ _ -> artistsURLDataToReport (specToBanner spec) j
