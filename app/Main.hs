{-# LANGUAGE OverloadedStrings, DeriveGeneric, PatternSynonyms #-}

module Main where

import LastSum.Types
import qualified LastSum.Settings as Settings
import qualified LastSum.Reporting as Reporting
import Control.Monad.Trans.Maybe


specFromSettings :: Settings.Settings -> Reporting.ReportSpec
specFromSettings settings = 
    let lastUsername = (Settings.lastUsername settings)
        period = (Settings.reportPeriod settings)
        limit = (Settings.reportLimit settings)
    in case (Settings.reportSubject settings) of
        TopArtists -> Reporting.TopArtistsReport lastUsername period limit
        TopAlbums -> Reporting.TopAlbumsReport lastUsername period limit
        TopTracks -> Reporting.TopTracksReport lastUsername period limit
        RecentTracks -> Reporting.RecentTracksReport lastUsername limit

main :: IO ()
main = do
    s <- Settings.getSettings
    case s of 
        Left err -> putStrLn err
        Right settings -> do
            r <- runMaybeT $ Reporting.getReport (Settings.lastAPIKey settings) (specFromSettings settings)
            putStrLn $ maybe "Unable to prepare report" id r
