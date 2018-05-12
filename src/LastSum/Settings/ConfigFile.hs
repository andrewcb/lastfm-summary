module LastSum.Settings.ConfigFile
where

import System.Directory
import qualified Data.ConfigFile as CF
import qualified Data.ConfigFile.Types as CFT
import Control.Monad.Except

data Config = Config { 
    lastUsername :: String, 
    lastAPIKey :: String
}

appName :: String
appName = "lastfm-summary"

configName :: IO String
configName = fmap (\d -> d ++ "/config") $ getAppUserDataDirectory appName

-- helper method for getting an option that may be absent
getMaybe :: (CF.Get_C a) => CF.ConfigParser -> CF.SectionSpec -> CF.OptionSpec -> Maybe a
getMaybe cp sect opt = do
        case (runExcept $ CF.get cp sect opt) of
            Right x -> Just x
            Left _ -> Nothing

-- TODO: make this return an Either
getConfig :: IO (Either String Config)
getConfig = do
    configFileName <- configName

    rv <- runExceptT $
        do
            cp <- join $ liftIO $ CF.readfile CF.emptyCP configFileName
            let x = cp

            username <- CF.get x "DEFAULT" "username"
            apiKey <- CF.get x "DEFAULT" "api_key"
            return $ Config username apiKey
    return $ case rv of
        Left err -> Left $ show err
        Right cfg -> Right cfg
