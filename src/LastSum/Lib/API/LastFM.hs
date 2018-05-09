module LastSum.Lib.API.LastFM 
where

data TopArtistsPeriod = Overall | P7Day | P1Month | P3Month | P6Month | P12Month deriving (Eq)

instance Show TopArtistsPeriod where
    show period = case period of 
        Overall -> "overall"
        P7Day -> "7day"
        P1Month -> "1month"
        P3Month -> "3month"
        P6Month -> "6month"
        P12Month -> "12month"


parseTopArtistsPeriod :: String -> Maybe TopArtistsPeriod
parseTopArtistsPeriod "overall" = Just Overall
parseTopArtistsPeriod ('7':'d':_) = Just P7Day
parseTopArtistsPeriod ('w':'e':'e':'k':_) = Just P7Day
parseTopArtistsPeriod ('1':'w':_) = Just P7Day
parseTopArtistsPeriod ('m':'o':'n':_) = Just P1Month
parseTopArtistsPeriod ('1':'m':_) = Just P1Month
parseTopArtistsPeriod ('3':'m':_) = Just P3Month
parseTopArtistsPeriod ('6':'m':_) = Just P6Month
parseTopArtistsPeriod ('1':'2':'m':_) = Just P12Month
parseTopArtistsPeriod ('1':'y':_) = Just P12Month
parseTopArtistsPeriod _ = Nothing
-- TODO: add more


-- The request type
data Request = UserTopArtists { user :: String, period :: TopArtistsPeriod, limit :: Int }

apiBase = "http://ws.audioscrobbler.com/2.0/"

requestURL :: String -> Request -> String
requestURL apiKey (UserTopArtists user period limit) = apiBase ++ "?method=user.gettopartists&user=" ++ user ++ "&api_key=" ++ apiKey ++ "&format=json&period=" ++ (show period) ++ "&limit=" ++ (show limit)

