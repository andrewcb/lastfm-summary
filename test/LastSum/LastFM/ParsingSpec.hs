{-# LANGUAGE QuasiQuotes #-}
module LastSum.LastFM.ParsingSpec (spec)
where

import Test.Hspec
import qualified Data.ByteString.Lazy as B
import LastSum.Lib.API.LastFM.Parsing
import Data.String.Quote

import Data.Aeson
import Data.Aeson.Types


main :: IO ()
main = hspec $ spec

spec = do
    describe "TopArtistsResponse" $ do
        it "parses a Top Artists response JSON block" $ do
            let r = parseArtistsResponse responseTopArtists
            r `shouldBe` Just [ArtistRecord "Mid-State Orange" 20, ArtistRecord "Tigercats" 20, ArtistRecord "Carpenter Brut" 18]

    describe "test" $ do
        it "runs tests" $ do
            let a = 2 + 2
            a `shouldBe` 4


responseTopArtists :: B.ByteString
responseTopArtists = [s|{"topartists":{"artist":[{"name":"Mid-State Orange","playcount":"20","mbid":"fbbce3d2-56f5-4139-aa98-d99416177552","url":"https://www.last.fm/music/Mid-State+Orange","streamable":"0","image":[{"#text":"https://lastfm-img2.akamaized.net/i/u/34s/6b7b66804ae24c4a988da948a835e2a8.png","size":"small"},{"#text":"https://lastfm-img2.akamaized.net/i/u/64s/6b7b66804ae24c4a988da948a835e2a8.png","size":"medium"},{"#text":"https://lastfm-img2.akamaized.net/i/u/174s/6b7b66804ae24c4a988da948a835e2a8.png","size":"large"},{"#text":"https://lastfm-img2.akamaized.net/i/u/300x300/6b7b66804ae24c4a988da948a835e2a8.png","size":"extralarge"},{"#text":"https://lastfm-img2.akamaized.net/i/u/300x300/6b7b66804ae24c4a988da948a835e2a8.png","size":"mega"}],"@attr":{"rank":"1"}},{"name":"Tigercats","playcount":"20","mbid":"3ff1fc20-3f0c-491b-b5cc-a7da66cd5efc","url":"https://www.last.fm/music/Tigercats","streamable":"0","image":[{"#text":"https://lastfm-img2.akamaized.net/i/u/34s/2b6c6b364afc4b3980f859ea2ed80139.png","size":"small"},{"#text":"https://lastfm-img2.akamaized.net/i/u/64s/2b6c6b364afc4b3980f859ea2ed80139.png","size":"medium"},{"#text":"https://lastfm-img2.akamaized.net/i/u/174s/2b6c6b364afc4b3980f859ea2ed80139.png","size":"large"},{"#text":"https://lastfm-img2.akamaized.net/i/u/300x300/2b6c6b364afc4b3980f859ea2ed80139.png","size":"extralarge"},{"#text":"https://lastfm-img2.akamaized.net/i/u/300x300/2b6c6b364afc4b3980f859ea2ed80139.png","size":"mega"}],"@attr":{"rank":"2"}},{"name":"Carpenter Brut","playcount":"18","mbid":"303abdee-ffca-4685-a582-6aaaefd81b01","url":"https://www.last.fm/music/Carpenter+Brut","streamable":"0","image":[{"#text":"https://lastfm-img2.akamaized.net/i/u/34s/f677e6220a571f003081ef3780b8d738.png","size":"small"},{"#text":"https://lastfm-img2.akamaized.net/i/u/64s/f677e6220a571f003081ef3780b8d738.png","size":"medium"},{"#text":"https://lastfm-img2.akamaized.net/i/u/174s/f677e6220a571f003081ef3780b8d738.png","size":"large"},{"#text":"https://lastfm-img2.akamaized.net/i/u/300x300/f677e6220a571f003081ef3780b8d738.png","size":"extralarge"},{"#text":"https://lastfm-img2.akamaized.net/i/u/300x300/f677e6220a571f003081ef3780b8d738.png","size":"mega"}],"@attr":{"rank":"3"}}]}}|] 