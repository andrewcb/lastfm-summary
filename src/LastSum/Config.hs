module LastSum.Config
where

import System.Directory
import qualified Data.ConfigFile as CF
import Control.Monad.Error

data Config = Config { lastUsername :: String, lastAPIKey :: String  }

appName :: String
appName = "lastfm-summary"

configName :: IO String
configName = fmap (\d -> d ++ "/config") $ getAppUserDataDirectory appName

--readConfigFile :: MonadError CF.CPError m => IO (m CF.ConfigParser) 
--readConfigFile = do 
--	dataDir <- getAppUserDataDirectory appName
--	return $ CF.readfile CF.emptyCP $ dataDir ++ "/config"


-- TODO: make this return an Either
--getConfig :: Error e => IO (Either e (String, String))
getConfig :: IO (Maybe Config)
getConfig = do
	configFileName <- configName

	rv <- runErrorT $
		do
			cp <- join $ liftIO $ CF.readfile CF.emptyCP configFileName
			let x = cp
			--return $ (\a b -> (a,b)) <$> (CF.get x "DEFAULT" "username") <*> (CF.get x "DEFAULT" "api_key")
 			username <- CF.get x "DEFAULT" "username"
			apiKey <- CF.get x "DEFAULT" "api_key"
			return $ Config username apiKey
	--return rv
	return $ case rv of
		Left _ -> Nothing
		Right cfg -> Just cfg
