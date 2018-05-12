module LastSum.Settings
where

import qualified LastSum.Settings.ConfigFile as SF
import qualified LastSum.Settings.CommandLine as SL
import qualified LastSum.Lib.API.LastFM as LastFM
import Data.Either.Utils

-- The app's unified runtime settings, which are aggregated from a 
-- variety of sources (configuration file, command-line options, &c.)

data Settings = Settings {
	lastUsername :: String,
	lastAPIKey :: String,
    reportPeriod :: LastFM.TopArtistsPeriod,
    reportLimit :: Int
}

getSettings :: IO (Either String Settings)
getSettings = do
	eitherCF <- SF.getConfig
	cl <- SL.getOptions
	return $ flip fmap eitherCF (\cf ->
		Settings (SF.lastUsername cf) (SF.lastAPIKey cf) (SL.optPeriod cl) (SL.optLimit cl))