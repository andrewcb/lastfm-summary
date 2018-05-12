module LastSum.Settings.CommandLine
where

import qualified LastSum.Lib.API.LastFM as LastFM
import qualified Options.Applicative as OPT
import Data.Semigroup ((<>))


data CommandLineOptions = CommandLineOptions {
    lastUsername :: Maybe String,
    period :: LastFM.RetroPeriod,
    limit :: Int
} deriving Show

parsePeriod :: OPT.ReadM LastFM.RetroPeriod
parsePeriod = OPT.eitherReader $ \s -> case LastFM.parseRetroPeriod s of
    Just p -> Right p
    Nothing -> Left "Invalid period specified"

commandLineParser :: OPT.Parser CommandLineOptions
commandLineParser = CommandLineOptions
    <$> OPT.option (OPT.maybeReader (\s -> Just $ Just s)) (
        OPT.long "last-username"
        <> OPT.short 'u'
        <> OPT.help "last.fm username to examine"
        <> OPT.value Nothing
        )
    <*> OPT.option parsePeriod (
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


getOptions :: IO CommandLineOptions
getOptions = OPT.execParser opts
    where
        opts = OPT.info (commandLineParser OPT.<**> OPT.helper)
            ( OPT.fullDesc 
                <> OPT.progDesc "produce a report from last.fm user listening data"
                <> OPT.header "lastfm-summary" )