{-# LANGUAGE OverloadedStrings, PatternSynonyms #-}

module LastSum.Reporting
where

import qualified LastSum.Settings as Settings
import qualified LastSum.Lib.API.LastFM as LastFM
import LastSum.Lib.API.LastFM.Parsing (
    ArtistPlayCount, pattern ArtistPlayCount, 
    ItemPlayCount, pattern ItemPlayCount, 
    RecentTrack, pattern RecentTrack,
    parseArtistsResponse, parseAlbumsResponse, parseTracksResponse, parseRecentTracksResponse)
import Network.HTTP.Conduit (simpleHttp)
import qualified Data.ByteString.Lazy as B
import Data.Either.Utils
import Control.Monad.Except
import Control.Monad.Trans.Maybe


data ReportSpec = TopArtistsReport String LastFM.RetroPeriod Int
    | TopAlbumsReport String LastFM.RetroPeriod Int
    | TopTracksReport String LastFM.RetroPeriod Int
    | RecentTracksReport String Int

makeTopArtistsReport :: String -> [ArtistPlayCount] -> String
makeTopArtistsReport banner topArtists = banner ++ body
    where
        body = foldr1 (\a b -> a++", "++b) $ fmap asString topArtists
        asString (ArtistPlayCount name count) = name ++ " (" ++ (show count) ++ ")"

makeTopItemsReport :: String -> [ItemPlayCount] -> String
makeTopItemsReport banner topItems = banner ++ body
    where
        body = foldr1 (\a b -> a++", "++b) $ fmap asString topItems
        asString (ItemPlayCount name artist count) = artist ++ " — " ++ name ++ " (" ++ (show count) ++ ")"

makeRecentTracksReport :: String -> [RecentTrack] -> String
makeRecentTracksReport banner recentTracks = banner ++ body
    where
        body = foldr1 (\a b -> a++", "++b) $ fmap asString recentTracks
        asString (RecentTrack name artist album) = artist ++ " — " ++ name 

artistsURLDataToReport :: String -> B.ByteString -> Maybe String
artistsURLDataToReport banner j = do
    ar <- parseArtistsResponse j
    return $ makeTopArtistsReport banner ar

albumsURLDataToReport :: String -> B.ByteString -> Maybe String
albumsURLDataToReport banner j = do
    ar <- parseAlbumsResponse j
    return $ makeTopItemsReport banner ar

tracksURLDataToReport :: String -> B.ByteString -> Maybe String
tracksURLDataToReport banner j = do
    ar <- parseTracksResponse j
    return $ makeTopItemsReport banner ar

recentTracksURLDataToReport :: String -> B.ByteString -> Maybe String
recentTracksURLDataToReport banner j = do
    r <- parseRecentTracksResponse j
    return $ makeRecentTracksReport banner r


specToRequest :: ReportSpec -> LastFM.Request
specToRequest (TopArtistsReport u p l) = LastFM.UserTopArtists u p l
specToRequest (TopAlbumsReport u p l) = LastFM.UserTopAlbums u p l
specToRequest (TopTracksReport u p l) = LastFM.UserTopTracks u p l
specToRequest (RecentTracksReport u l) = LastFM.UserRecentTracks u l

specToBanner :: ReportSpec -> String
specToBanner (TopArtistsReport u p l) = "Top last.fm artists of " ++ LastFM.descriptiveName p ++ ": "
specToBanner (TopAlbumsReport u p l) = "Top last.fm albums of " ++ LastFM.descriptiveName p ++ ": "
specToBanner (TopTracksReport u p l) = "Top last.fm tracks of " ++ LastFM.descriptiveName p ++ ": "
specToBanner (RecentTracksReport u l) = "Most recently listened tracks:"

getReport :: String -> ReportSpec -> MaybeT IO String
getReport apiKey spec = do
    --config <- MaybeT $ Config.getConfig
    let url = LastFM.requestURL apiKey $ specToRequest spec
    j <- liftIO $ simpleHttp url
    MaybeT $ return $ case spec of
        TopArtistsReport _ _ _ -> artistsURLDataToReport (specToBanner spec) j
        TopAlbumsReport _ _ _ -> albumsURLDataToReport (specToBanner spec) j
        TopTracksReport _ _ _ -> tracksURLDataToReport (specToBanner spec) j
        RecentTracksReport _ _ -> recentTracksURLDataToReport (specToBanner spec) j
