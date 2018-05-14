{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module LastSum.Lib.API.LastFM.Parsing
where

import qualified LastSum.Lib.API.LastFM as LastFM

import qualified Data.ByteString.Lazy as B
import Data.Aeson
import Data.Aeson.Types
import Text.Read (readMaybe)
import Data.Text as T

data ArtistPlayCount = ArtistPlayCount {
    artistName :: String,
    artistPlays :: Int
} deriving (Eq, Show)

-- a work of an artist; here, it encompasses tracks and albums
data WorkPlayCount = WorkPlayCount {
    itemName :: String,
    itemArtistName :: String,
    itemPlays :: Int
} deriving (Eq, Show)

data RecentTrack = RecentTrack {
    recentTrackName :: String,
    -- TODO: make these optional
    recentTrackArtist :: String,
    recentTrackAlbum :: String
    -- TODO: add date 
} deriving (Eq, Show)


instance FromJSON ArtistPlayCount where
    parseJSON = withObject "artistRecord" $ \o -> do
        name <- o .: "name"
        countStr <- o .: "playcount"
        case readMaybe countStr :: Maybe Int of
            Just c -> return $ ArtistPlayCount name c
            Nothing -> fail "No well-formed playcount"

instance FromJSON WorkPlayCount where
    parseJSON = withObject "itemRecord" $ \o -> do
        name <- o .: "name"
        artist <- o .: "artist"
        artistName <- artist .: "name"
        countStr <- o .: "playcount"
        case readMaybe countStr :: Maybe Int of
            Just c -> return $ WorkPlayCount name artistName c
            Nothing -> fail "No well-formed playcount"

instance FromJSON RecentTrack where
    parseJSON = withObject "recentTrack" $ \o -> do
        name <- o .: "name"
        artistRec <- o .: "artist"
        artistName <- artistRec .: "#text"
        albumRec <- o .: "album"
        albumName <- albumRec .: "#text"
        return $ RecentTrack name artistName albumName

topArtistsResponse = withObject "topartists" $ \o -> do
    p <- o .: "topartists"
    artists <- p .: "artist"
    return artists :: Parser [ArtistPlayCount]

topWorksResponse :: String -> Value -> Parser [WorkPlayCount]
topWorksResponse itemType = withObject subject $ \o -> do
    p <- o .: (T.pack subject)
    items <- p .: (T.pack itemType)
    return items :: Parser [WorkPlayCount]
    where
        subject =  "top" ++ itemType ++ "s"

recentTracksResponse = withObject "recentTracks" $ \o -> do
    p <- o .: "recenttracks"
    tracks <- p .: "track"
    return tracks :: Parser [RecentTrack]

parseArtistsResponse :: B.ByteString -> Maybe [ArtistPlayCount]
parseArtistsResponse j = decode j >>= parseMaybe topArtistsResponse

parseAlbumsResponse :: B.ByteString -> Maybe [WorkPlayCount]
parseAlbumsResponse j = decode j >>= (parseMaybe (topWorksResponse "album"))

parseTracksResponse :: B.ByteString -> Maybe [WorkPlayCount]
parseTracksResponse j = decode j >>= (parseMaybe (topWorksResponse "track"))

parseRecentTracksResponse :: B.ByteString -> Maybe [RecentTrack]
parseRecentTracksResponse j = decode j >>= parseMaybe recentTracksResponse