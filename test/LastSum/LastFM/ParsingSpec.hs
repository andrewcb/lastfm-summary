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
            r `shouldBe` Just [ArtistPlayCount "Mid-State Orange" 20, ArtistPlayCount "Tigercats" 20, ArtistPlayCount "Carpenter Brut" 18]

    describe "TopAlbumsResponse" $ do
        it "parses a Top Albums response JSON block" $ do
            let r = parseAlbumsResponse responseTopAlbums
            r `shouldBe` Just [ItemPlayCount "Performer" "Montero" 187, ItemPlayCount "Pig City" "Tigercats" 50, ItemPlayCount "Earth Loop" "Moon Gangs" 48]

    describe "TopTracksResponse" $ do
        it "parses a Top Tracks response JSON block" $ do
            let r = parseTracksResponse responseTopTracks
            r `shouldBe` Just [ItemPlayCount "Aloha" "Montero" 31, ItemPlayCount "Caught Up in My Own World" "Montero" 27, ItemPlayCount "Montero Airlines" "Montero" 26]

    describe "test" $ do
        it "runs tests" $ do
            let a = 2 + 2
            a `shouldBe` 4


responseTopArtists :: B.ByteString
responseTopArtists = [s|{"topartists":{"artist":[{"name":"Mid-State Orange","playcount":"20","mbid":"fbbce3d2-56f5-4139-aa98-d99416177552","url":"https://www.last.fm/music/Mid-State+Orange","streamable":"0","image":[{"#text":"https://lastfm-img2.akamaized.net/i/u/34s/6b7b66804ae24c4a988da948a835e2a8.png","size":"small"},{"#text":"https://lastfm-img2.akamaized.net/i/u/64s/6b7b66804ae24c4a988da948a835e2a8.png","size":"medium"},{"#text":"https://lastfm-img2.akamaized.net/i/u/174s/6b7b66804ae24c4a988da948a835e2a8.png","size":"large"},{"#text":"https://lastfm-img2.akamaized.net/i/u/300x300/6b7b66804ae24c4a988da948a835e2a8.png","size":"extralarge"},{"#text":"https://lastfm-img2.akamaized.net/i/u/300x300/6b7b66804ae24c4a988da948a835e2a8.png","size":"mega"}],"@attr":{"rank":"1"}},{"name":"Tigercats","playcount":"20","mbid":"3ff1fc20-3f0c-491b-b5cc-a7da66cd5efc","url":"https://www.last.fm/music/Tigercats","streamable":"0","image":[{"#text":"https://lastfm-img2.akamaized.net/i/u/34s/2b6c6b364afc4b3980f859ea2ed80139.png","size":"small"},{"#text":"https://lastfm-img2.akamaized.net/i/u/64s/2b6c6b364afc4b3980f859ea2ed80139.png","size":"medium"},{"#text":"https://lastfm-img2.akamaized.net/i/u/174s/2b6c6b364afc4b3980f859ea2ed80139.png","size":"large"},{"#text":"https://lastfm-img2.akamaized.net/i/u/300x300/2b6c6b364afc4b3980f859ea2ed80139.png","size":"extralarge"},{"#text":"https://lastfm-img2.akamaized.net/i/u/300x300/2b6c6b364afc4b3980f859ea2ed80139.png","size":"mega"}],"@attr":{"rank":"2"}},{"name":"Carpenter Brut","playcount":"18","mbid":"303abdee-ffca-4685-a582-6aaaefd81b01","url":"https://www.last.fm/music/Carpenter+Brut","streamable":"0","image":[{"#text":"https://lastfm-img2.akamaized.net/i/u/34s/f677e6220a571f003081ef3780b8d738.png","size":"small"},{"#text":"https://lastfm-img2.akamaized.net/i/u/64s/f677e6220a571f003081ef3780b8d738.png","size":"medium"},{"#text":"https://lastfm-img2.akamaized.net/i/u/174s/f677e6220a571f003081ef3780b8d738.png","size":"large"},{"#text":"https://lastfm-img2.akamaized.net/i/u/300x300/f677e6220a571f003081ef3780b8d738.png","size":"extralarge"},{"#text":"https://lastfm-img2.akamaized.net/i/u/300x300/f677e6220a571f003081ef3780b8d738.png","size":"mega"}],"@attr":{"rank":"3"}}]}}|] 

responseTopAlbums :: B.ByteString
responseTopAlbums = [s|{"topalbums":{"album":[{"name":"Performer","playcount":"187","mbid":"","url":"https://www.last.fm/music/Montero/Performer","artist":{"name":"Montero","mbid":"ab95b04a-567b-4d54-b351-afbcd3f62ae1","url":"https://www.last.fm/music/Montero"},"image":[{"#text":"https://lastfm-img2.akamaized.net/i/u/34s/a4ff16dfb4dd1716b63c992890fcd365.png","size":"small"},{"#text":"https://lastfm-img2.akamaized.net/i/u/64s/a4ff16dfb4dd1716b63c992890fcd365.png","size":"medium"},{"#text":"https://lastfm-img2.akamaized.net/i/u/174s/a4ff16dfb4dd1716b63c992890fcd365.png","size":"large"},{"#text":"https://lastfm-img2.akamaized.net/i/u/300x300/a4ff16dfb4dd1716b63c992890fcd365.png","size":"extralarge"}],"@attr":{"rank":"1"}},{"name":"Pig City","playcount":"50","mbid":"","url":"https://www.last.fm/music/Tigercats/Pig+City","artist":{"name":"Tigercats","mbid":"3ff1fc20-3f0c-491b-b5cc-a7da66cd5efc","url":"https://www.last.fm/music/Tigercats"},"image":[{"#text":"https://lastfm-img2.akamaized.net/i/u/34s/ff73fb6ce96a19172f71253a35b06797.png","size":"small"},{"#text":"https://lastfm-img2.akamaized.net/i/u/64s/ff73fb6ce96a19172f71253a35b06797.png","size":"medium"},{"#text":"https://lastfm-img2.akamaized.net/i/u/174s/ff73fb6ce96a19172f71253a35b06797.png","size":"large"},{"#text":"https://lastfm-img2.akamaized.net/i/u/300x300/ff73fb6ce96a19172f71253a35b06797.png","size":"extralarge"}],"@attr":{"rank":"2"}},{"name":"Earth Loop","playcount":"48","mbid":"","url":"https://www.last.fm/music/Moon+Gangs/Earth+Loop","artist":{"name":"Moon Gangs","mbid":"","url":"https://www.last.fm/music/Moon+Gangs"},"image":[{"#text":"https://lastfm-img2.akamaized.net/i/u/34s/19f2c208b8853bb7241fc4f0008aa101.png","size":"small"},{"#text":"https://lastfm-img2.akamaized.net/i/u/64s/19f2c208b8853bb7241fc4f0008aa101.png","size":"medium"},{"#text":"https://lastfm-img2.akamaized.net/i/u/174s/19f2c208b8853bb7241fc4f0008aa101.png","size":"large"},{"#text":"https://lastfm-img2.akamaized.net/i/u/300x300/19f2c208b8853bb7241fc4f0008aa101.png","size":"extralarge"}],"@attr":{"rank":"3"}}]}}|]

responseTopTracks :: B.ByteString
responseTopTracks = [s|{"toptracks":{"track":[{"name":"Aloha","duration":"0","playcount":"31","mbid":"","url":"https://www.last.fm/music/Montero/_/Aloha","streamable":{"#text":"0","fulltrack":"0"},"artist":{"name":"Montero","mbid":"ab95b04a-567b-4d54-b351-afbcd3f62ae1","url":"https://www.last.fm/music/Montero"},"image":[{"#text":"https://lastfm-img2.akamaized.net/i/u/34s/46021f8472bc4a9cb43a1a3238b9b46b.png","size":"small"},{"#text":"https://lastfm-img2.akamaized.net/i/u/64s/46021f8472bc4a9cb43a1a3238b9b46b.png","size":"medium"},{"#text":"https://lastfm-img2.akamaized.net/i/u/174s/46021f8472bc4a9cb43a1a3238b9b46b.png","size":"large"},{"#text":"https://lastfm-img2.akamaized.net/i/u/300x300/46021f8472bc4a9cb43a1a3238b9b46b.png","size":"extralarge"}],"@attr":{"rank":"1"}},{"name":"Caught Up in My Own World","duration":"0","playcount":"27","mbid":"","url":"https://www.last.fm/music/Montero/_/Caught+Up+in+My+Own+World","streamable":{"#text":"0","fulltrack":"0"},"artist":{"name":"Montero","mbid":"ab95b04a-567b-4d54-b351-afbcd3f62ae1","url":"https://www.last.fm/music/Montero"},"image":[{"#text":"https://lastfm-img2.akamaized.net/i/u/34s/46021f8472bc4a9cb43a1a3238b9b46b.png","size":"small"},{"#text":"https://lastfm-img2.akamaized.net/i/u/64s/46021f8472bc4a9cb43a1a3238b9b46b.png","size":"medium"},{"#text":"https://lastfm-img2.akamaized.net/i/u/174s/46021f8472bc4a9cb43a1a3238b9b46b.png","size":"large"},{"#text":"https://lastfm-img2.akamaized.net/i/u/300x300/46021f8472bc4a9cb43a1a3238b9b46b.png","size":"extralarge"}],"@attr":{"rank":"2"}},{"name":"Montero Airlines","duration":"0","playcount":"26","mbid":"","url":"https://www.last.fm/music/Montero/_/Montero+Airlines","streamable":{"#text":"0","fulltrack":"0"},"artist":{"name":"Montero","mbid":"ab95b04a-567b-4d54-b351-afbcd3f62ae1","url":"https://www.last.fm/music/Montero"},"image":[{"#text":"https://lastfm-img2.akamaized.net/i/u/34s/46021f8472bc4a9cb43a1a3238b9b46b.png","size":"small"},{"#text":"https://lastfm-img2.akamaized.net/i/u/64s/46021f8472bc4a9cb43a1a3238b9b46b.png","size":"medium"},{"#text":"https://lastfm-img2.akamaized.net/i/u/174s/46021f8472bc4a9cb43a1a3238b9b46b.png","size":"large"},{"#text":"https://lastfm-img2.akamaized.net/i/u/300x300/46021f8472bc4a9cb43a1a3238b9b46b.png","size":"extralarge"}],"@attr":{"rank":"3"}}]}}|]