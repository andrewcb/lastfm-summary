module LastSum.Types
where

data ReportSubject = TopArtists | TopAlbums | TopTracks | RecentTracks deriving (Eq, Show)

parseReportSubject :: String -> Maybe ReportSubject
parseReportSubject ('t':'o':'p':xs) = parseRest xs
    where
        parseRest ('a':'r':'t':_) = Just TopArtists
        parseRest ('a':'l':'b':_) = Just TopAlbums
        parseRest ('t':'r':'a':_) = Just TopTracks
        parseRest _ = Nothing
parseReportSubject ('r':'e':'c':'e':'n':'t':_) = Just RecentTracks
parseReportSubject _ = Nothing

reportSubjectNames = [ "topartists", "topalbums", "toptracks", "recent"]