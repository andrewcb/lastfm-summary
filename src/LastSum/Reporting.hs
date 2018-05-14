{-# LANGUAGE OverloadedStrings, PatternSynonyms #-}

module LastSum.Reporting
where

import qualified LastSum.Settings as Settings
import qualified LastSum.Lib.API.LastFM as LastFM
import LastSum.Lib.API.LastFM.Parsing (
    ArtistPlayCount, pattern ArtistPlayCount, 
    WorkPlayCount, pattern WorkPlayCount, 
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

makeReportForItems :: String -> [a] -> (a -> String) -> String
makeReportForItems banner items formatter = banner ++ (foldr1 (\a b -> a++", "++b) $ fmap formatter items)

makeTopArtistsReport banner topArtists = makeReportForItems banner topArtists (\(ArtistPlayCount name count) -> name ++ " (" ++ (show count) ++ ")")
makeTopWorksReport banner topItems = makeReportForItems banner topItems (\(WorkPlayCount name artist count) -> artist ++ " — " ++ name ++ " (" ++ (show count) ++ ")")
makeRecentTracksReport banner recentTracks = makeReportForItems banner recentTracks (\(RecentTrack name artist album) -> artist ++ " — " ++ name)

artistsURLDataToReport :: String -> B.ByteString -> Maybe String
artistsURLDataToReport banner = fmap (makeTopArtistsReport banner) . parseArtistsResponse

albumsURLDataToReport :: String -> B.ByteString -> Maybe String
albumsURLDataToReport banner = fmap (makeTopWorksReport banner) . parseAlbumsResponse

tracksURLDataToReport :: String -> B.ByteString -> Maybe String
tracksURLDataToReport banner = fmap (makeTopWorksReport banner) . parseTracksResponse

recentTracksURLDataToReport :: String -> B.ByteString -> Maybe String
recentTracksURLDataToReport banner = fmap (makeRecentTracksReport banner) . parseRecentTracksResponse


specToRequest :: ReportSpec -> LastFM.Request
specToRequest (TopArtistsReport u p l) = LastFM.UserTopArtists u p l
specToRequest (TopAlbumsReport u p l) = LastFM.UserTopAlbums u p l
specToRequest (TopTracksReport u p l) = LastFM.UserTopTracks u p l
specToRequest (RecentTracksReport u l) = LastFM.UserRecentTracks u l

specToBanner :: ReportSpec -> String
specToBanner (TopArtistsReport u p l) = "Top last.fm artists of " ++ LastFM.descriptiveName p ++ ": "
specToBanner (TopAlbumsReport u p l) = "Top last.fm albums of " ++ LastFM.descriptiveName p ++ ": "
specToBanner (TopTracksReport u p l) = "Top last.fm tracks of " ++ LastFM.descriptiveName p ++ ": "
specToBanner (RecentTracksReport u l) = "Most recently listened tracks: "

getReport :: String -> ReportSpec -> MaybeT IO String
getReport apiKey spec = do
    let url = LastFM.requestURL apiKey $ specToRequest spec
    j <- liftIO $ simpleHttp url
    let banner = specToBanner spec
    MaybeT $ return $ case spec of
        TopArtistsReport _ _ _ -> artistsURLDataToReport banner j
        TopAlbumsReport _ _ _ -> albumsURLDataToReport banner j
        TopTracksReport _ _ _ -> tracksURLDataToReport banner j
        RecentTracksReport _ _ -> recentTracksURLDataToReport banner j
