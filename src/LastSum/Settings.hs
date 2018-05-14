module LastSum.Settings
where

import LastSum.Types
import qualified LastSum.Settings.ConfigFile as SF
import qualified LastSum.Settings.CommandLine as SL
import qualified LastSum.Lib.API.LastFM as LastFM
import Control.Applicative

-- The app's unified runtime settings, which are aggregated from a 
-- variety of sources (configuration file, command-line options, &c.)

data Settings = Settings {
    lastUsername :: String,
    lastAPIKey :: String,
    reportSubject :: ReportSubject,
    reportPeriod :: LastFM.RetroPeriod,
    reportLimit :: Int
}

-- Swift-like ?? operator, for convenience
infixr 4 ??
(??) :: Maybe a -> a -> a
(??) a b = maybe b id a

getSettings :: IO (Either String Settings)
getSettings = do
    eitherCF <- SF.getConfig
    cl <- SL.getOptions
    return $ flip fmap eitherCF (\cf ->
        let lastUsername = SL.lastUsername cl ?? SF.lastUsername cf
        in Settings lastUsername (SF.lastAPIKey cf) (SL.subject cl) (SL.period cl) (SL.limit cl))