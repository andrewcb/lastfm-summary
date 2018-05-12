{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module LastSum.Lib.API.LastFM.Parsing
where

import qualified LastSum.Lib.API.LastFM as LastFM

import qualified Data.ByteString.Lazy as B
import Data.Aeson
import Data.Aeson.Types
import Text.Read (readMaybe)

data ArtistRecord = ArtistRecord {
    name :: String,
    playcount :: Int
} deriving (Show)

topArtistsResponse = withObject "topartists" $ \o -> do
    p <- o .: "topartists"
    artists <- p .: "artist"
    return artists :: Parser [ArtistRecord]

instance FromJSON ArtistRecord where
    parseJSON = withObject "artistRecord" $ \o -> do
        name <- o .: "name"
        countStr <- o .: "playcount"
        case readMaybe countStr :: Maybe Int of
            Just c -> return $ ArtistRecord name c
            Nothing -> fail "No well-formed playcount"


parseArtistsResponse :: B.ByteString -> Maybe [ArtistRecord]
parseArtistsResponse j = decode j >>= parseMaybe topArtistsResponse