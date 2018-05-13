{-# LANGUAGE PatternSynonyms #-}

module LastSum.Lib.API.LastFM 
where

--import LastSum.Lib.API.LastFM.Types

data RetroPeriod = Overall | P7Day | P1Month | P3Month | P6Month | P12Month deriving (Eq)

instance Show RetroPeriod where
    show period = case period of 
        Overall -> "overall"
        P7Day -> "7day"
        P1Month -> "1month"
        P3Month -> "3month"
        P6Month -> "6month"
        P12Month -> "12month"

descriptiveName :: RetroPeriod -> String
descriptiveName Overall = "all time"
descriptiveName P7Day = "the past week"
descriptiveName P1Month = "the past month"
descriptiveName P3Month = "the past 3 months"
descriptiveName P6Month = "the past 6 months"
descriptiveName P12Month = "the past year"
-- TODO: investigate localisation

parseRetroPeriod :: String -> Maybe RetroPeriod
parseRetroPeriod "overall" = Just Overall
parseRetroPeriod ('7':'d':_) = Just P7Day
parseRetroPeriod ('w':'e':'e':'k':_) = Just P7Day
parseRetroPeriod ('1':'w':_) = Just P7Day
parseRetroPeriod ('m':'o':'n':_) = Just P1Month
parseRetroPeriod ('1':'m':_) = Just P1Month
parseRetroPeriod ('3':'m':_) = Just P3Month
parseRetroPeriod ('6':'m':_) = Just P6Month
parseRetroPeriod ('1':'2':'m':_) = Just P12Month
parseRetroPeriod ('1':'y':_) = Just P12Month
parseRetroPeriod _ = Nothing


-- The request type
data Request = UserTopArtists { user :: String, period :: RetroPeriod, limit :: Int }
    | UserTopAlbums { user :: String, period :: RetroPeriod, limit :: Int }
    | UserTopTracks { user :: String, period :: RetroPeriod, limit :: Int }
    | UserRecentTracks { user :: String, limit :: Int } -- TODO: add from/to timestamps

apiBase = "http://ws.audioscrobbler.com/2.0/"

requestURL :: String -> Request -> String
requestURL apiKey (UserTopArtists user period limit) = apiBase ++ "?method=user.gettopartists&user=" ++ user ++ "&api_key=" ++ apiKey ++ "&format=json&period=" ++ (show period) ++ "&limit=" ++ (show limit)
requestURL apiKey (UserTopAlbums user period limit) = apiBase ++ "?method=user.gettopalbums&user=" ++ user ++ "&api_key=" ++ apiKey ++ "&format=json&period=" ++ (show period) ++ "&limit=" ++ (show limit)
requestURL apiKey (UserTopTracks user period limit) = apiBase ++ "?method=user.gettoptracks&user=" ++ user ++ "&api_key=" ++ apiKey ++ "&format=json&period=" ++ (show period) ++ "&limit=" ++ (show limit)
requestURL apiKey (UserRecentTracks user limit) = apiBase ++ "?method=user.getrecenttracks&user=" ++ user ++ "&api_key=" ++ apiKey ++ "&format=json&limit=" ++ (show limit)
