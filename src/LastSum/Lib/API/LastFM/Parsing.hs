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

data ItemPlayCount = ItemPlayCount {
    itemName :: String,
    itemArtistName :: String,
    itemPlays :: Int
} deriving (Eq, Show)


instance FromJSON ArtistPlayCount where
    parseJSON = withObject "artistRecord" $ \o -> do
        name <- o .: "name"
        countStr <- o .: "playcount"
        case readMaybe countStr :: Maybe Int of
            Just c -> return $ ArtistPlayCount name c
            Nothing -> fail "No well-formed playcount"

instance FromJSON ItemPlayCount where
    parseJSON = withObject "artistRecord" $ \o -> do
        name <- o .: "name"
        artist <- o .: "artist"
        artistName <- artist .: "name"
        countStr <- o .: "playcount"
        case readMaybe countStr :: Maybe Int of
            Just c -> return $ ItemPlayCount name artistName c
            Nothing -> fail "No well-formed playcount"

topArtistsResponse = withObject "topartists" $ \o -> do
    p <- o .: "topartists"
    artists <- p .: "artist"
    return artists :: Parser [ArtistPlayCount]

topItemsResponse :: String -> Value -> Parser [ItemPlayCount]
topItemsResponse itemType = withObject subject $ \o -> do
    p <- o .: (T.pack subject)
    items <- p .: (T.pack itemType)
    return items :: Parser [ItemPlayCount]
    where
        subject =  "top" ++ itemType ++ "s"

parseArtistsResponse :: B.ByteString -> Maybe [ArtistPlayCount]
parseArtistsResponse j = decode j >>= parseMaybe topArtistsResponse

parseAlbumsResponse :: B.ByteString -> Maybe [ItemPlayCount]
parseAlbumsResponse j = decode j >>= (parseMaybe (topItemsResponse "album"))

parseTracksResponse :: B.ByteString -> Maybe [ItemPlayCount]
parseTracksResponse j = decode j >>= (parseMaybe (topItemsResponse "track"))