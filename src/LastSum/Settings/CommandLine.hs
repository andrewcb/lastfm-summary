module LastSum.Settings.CommandLine
where

import qualified LastSum.Lib.API.LastFM as LastFM
import LastSum.Types
import qualified Options.Applicative as OPT
import Data.Semigroup ((<>))


data CommandLineOptions = CommandLineOptions {
    lastUsername :: Maybe String,
    period :: LastFM.RetroPeriod,
    limit :: Int,
    subject :: ReportSubject
} deriving Show

validSubjects = foldr1 (\a b -> a++", "++b) reportSubjectNames
validPeriods = foldr1 (\a b -> a++", "++b)  LastFM.retroPeriodNames


parseSubject :: OPT.ReadM ReportSubject
parseSubject = OPT.eitherReader $ \s -> case parseReportSubject s of
    Just s -> Right s
    Nothing -> Left $ "Invalid subject specified; should be one of: " ++ validSubjects

parsePeriod :: OPT.ReadM LastFM.RetroPeriod
parsePeriod = OPT.eitherReader $ \s -> case LastFM.parseRetroPeriod s of
    Just p -> Right p
    Nothing -> Left $ "Invalid period specified; should be one of: " ++ validPeriods

commandLineParser :: OPT.Parser CommandLineOptions
commandLineParser = CommandLineOptions
    <$> OPT.option (OPT.maybeReader (\s -> Just $ Just s)) (
        OPT.long "last-username"
        <> OPT.short 'u'
        <> OPT.metavar "NAME"
        <> OPT.help "the username of the last.fm feed to examine"
        <> OPT.value Nothing
        )
    <*> OPT.option parsePeriod (
        OPT.long "period" 
        <> OPT.short 'p' 
        <> OPT.help ("period of report; one of: " ++ validPeriods)
        <> OPT.metavar "P"
        <> OPT.value LastFM.P7Day )
    <*> OPT.option OPT.auto (
        OPT.long "limit"
        <> OPT.short 'l'
        <> OPT.help "number of entries to list"
        <> OPT.metavar "N"
        <> OPT.value 5
        <> OPT.showDefault )
    <*> OPT.argument parseSubject (
        OPT.metavar "subject"  )


getOptions :: IO CommandLineOptions
getOptions = OPT.execParser opts
    where
        opts = OPT.info (commandLineParser OPT.<**> OPT.helper)
            ( OPT.fullDesc 
                <> OPT.progDesc ("Produce a report from last.fm user listening data. The subject can be one of: " ++ validSubjects)
                <> OPT.header "lastfm-summary" )