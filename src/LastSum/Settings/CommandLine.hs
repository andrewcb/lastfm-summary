module LastSum.Settings.CommandLine
where

import qualified LastSum.Lib.API.LastFM as LastFM
import LastSum.Types
import qualified Options.Applicative as OPT
import Data.Semigroup ((<>))


data CommandLineOptions = CommandLineOptions {
    lastUsername :: Maybe String,
    subject :: ReportSubject,
    period :: LastFM.RetroPeriod,
    limit :: Int
} deriving Show

validSubjects = foldr1 (\a b -> a++", "++b) reportSubjectNames
validPeriods = foldr1 (\a b -> a++", "++b)  LastFM.retroPeriodNames


parseSubject :: OPT.ReadM ReportSubject
parseSubject = OPT.eitherReader $ \s -> case parseReportSubject s of
    Just s -> Right s
    Nothing -> Left $ "Invalid subject specified; should be one of " ++ validSubjects

parsePeriod :: OPT.ReadM LastFM.RetroPeriod
parsePeriod = OPT.eitherReader $ \s -> case LastFM.parseRetroPeriod s of
    Just p -> Right p
    Nothing -> Left $ "Invalid period specified; should be one of " ++ validPeriods

commandLineParser :: OPT.Parser CommandLineOptions
commandLineParser = CommandLineOptions
    <$> OPT.option (OPT.maybeReader (\s -> Just $ Just s)) (
        OPT.long "last-username"
        <> OPT.short 'u'
        <> OPT.help "last.fm username to examine"
        <> OPT.value Nothing
        )
    <*> OPT.option parseSubject (
        OPT.long "what"
        <> OPT.short 'w'
        <> OPT.help ("the data to report; one of " ++ validSubjects)
        <> OPT.value RecentTracks )
    <*> OPT.option parsePeriod (
        OPT.long "period" 
        <> OPT.short 'p' 
        <> OPT.help ("period of report; one of " ++ validPeriods)
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