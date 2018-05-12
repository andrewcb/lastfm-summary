module LastSum.Settings
where

import qualified LastSum.Settings.ConfigFile as SF
import qualified LastSum.Settings.CommandLine as SL
import qualified LastSum.Lib.API.LastFM as LastFM

-- The app's unified runtime settings, which are aggregated from a 
-- variety of sources (configuration file, command-line options, &c.)

data Settings = Settings {
	lastUsername :: String,
	lastAPIKey :: String,
    reportPeriod :: LastFM.TopArtistsPeriod,
    reportLimit :: Int
}

getSettings :: IO (Maybe Settings)
getSettings = do
	maybeCF <- SF.getConfig
	cl <- SL.getOptions
	return $ flip fmap maybeCF (\cf ->
		Settings (SF.lastUsername cf) (SF.lastAPIKey cf) (SL.optPeriod cl) (SL.optLimit cl))