module Main (main, humanOrder) where

import Data.Char
import Data.String
import qualified Data.List as List
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Network.HTTP.Simple as HTTP
import qualified Network.HTTP.Types.Status as HTTP.Status
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai as Wai
import System.Directory
import System.Exit
import System.IO
import System.IO.Temp
import System.Process.Typed
import Text.Printf

dbUrl :: String
dbUrl = "http://localhost:2718"

dbFilesPath :: String
dbFilesPath = "/files"

dbTxPath :: String
dbTxPath = "/database/transact"

sendFile :: FilePath -> IO (Maybe LBS.ByteString)
sendFile path = do
  req <- HTTP.parseRequest ("PUT " ++ dbUrl ++ dbFilesPath)
  let req' = HTTP.setRequestBodyFile path req
  resp <- HTTP.httpLBS req'
  case HTTP.getResponseStatusCode resp of
    200 -> return (Just (HTTP.getResponseBody resp))
    _ -> return Nothing

sendTx :: LBS.ByteString -> IO (Maybe ())
sendTx tx = do
  req <- HTTP.parseRequest ("POST " ++ dbUrl ++ dbTxPath)
  let req' = HTTP.setRequestBodyLBS tx req
  resp <- HTTP.httpLBS req'
  case HTTP.getResponseStatusCode resp of
    200 -> return (Just ())
    _ -> do
      putStrLn "Bad Tx: "
      LBS.putStr tx
      putStrLn ""
      putStrLn "Response: "
      LBS.putStr (HTTP.getResponseBody resp)
      putStrLn ""
      return Nothing

-- Given a list of QStore filehandle strings, returns the string representation of a transaction which will create a new album for those files
newAlbumTx :: [String] -> [LBS.ByteString] -> LBS.ByteString
newAlbumTx names files =
  LBS.intercalate "\n" $ List.concat
    [ ["("]
    , [LBS.intercalate " " ["(", "album", "\"is\"", "\"album\"", "#t", ")"]]
    , [LBS.intercalate " " ["(", "album", "\"name\"", "\"New Album\"", "#t", ")"]]
    , List.concat (zipWith (newSongTxElem "album") [1..] (zip names files))
    , [")"]
    ]

newSongTxElem :: LBS.ByteString -> Int -> (String, LBS.ByteString) -> [LBS.ByteString]
newSongTxElem album n (name, file) =
  [ LBS.intercalate " " ["(", album, "\"includes track\"", track, "#t", ")"]
  , LBS.intercalate " " ["(", track, "\"is\"", "\"track\"", "#t", ")"]
  , LBS.intercalate " " ["(", track, "\"number\"", fromString (show n), "#t", ")"]
  , LBS.intercalate " " ["(", track, "\"song\"", song, "#t", ")"]
  , LBS.intercalate " " ["(", song, "\"is\"", "\"song\"", "#t", ")"]
  , LBS.intercalate " " ["(", song, "\"name\"", fromString (show name), "#t", ")"]
  , LBS.intercalate " " ["(", song, "\"file\"", file, "#t", ")"]
  ]
  where
    track = LBS.append "track" (fromString (show n))
    song = LBS.append "song" (fromString (show n))

readMaybe :: Read a => String -> Maybe a
readMaybe s
  | [(v, _)] <- readsPrec 0 s = Just v
  | otherwise = Nothing

splitInt :: String -> (Int, String)
splitInt s = loop "" s
  where
    loop :: String -> String -> (Int, String)
    loop intS [] = (maybe 0 id (readMaybe (reverse intS)), "")
    loop intS (c : s')
      | isNumber c = loop (c : intS) s'
      | otherwise = (read (reverse intS), c : s')

humanOrder :: String -> String -> Ordering
humanOrder [] [] = EQ
humanOrder [] _ = LT
humanOrder _ [] = GT
humanOrder (aC : aS) (bC : bS)
  | isNumber aC || isNumber bC =
    let
      (aInt, aS') = splitInt (aC : aS)
      (bInt, bS') = splitInt (bC : bS)
    in
      case compare aInt bInt of
        EQ -> humanOrder aS' bS'
        o -> o
  | otherwise =
    case compare aC bC of
      EQ -> humanOrder aS bS
      o -> o

runYtdlp :: String -> (Wai.Response -> IO Wai.ResponseReceived) -> IO Wai.ResponseReceived
runYtdlp url resp = withSystemTempDirectory "qstore-ytdlp" \tmpDirPath -> do
  exitCode <- runProcess (fromString (printf "yt-dlp --no-playlist -x --force-overwrite --audio-format mp3 -o '%s/%%(playlist_autonumber)s-%%(title)s.%%(ext)s' '%s'" tmpDirPath url))
  case exitCode of
    ExitFailure _ -> resp (Wai.responseLBS HTTP.Status.status400 [] "Couldn't download link")
    ExitSuccess -> do
      songNames <- List.sortBy humanOrder <$> listDirectory tmpDirPath
      let songPaths = ((tmpDirPath ++ "/") ++) <$> songNames
      result <- sequence <$> sequence (map sendFile songPaths)
      case result of
        Nothing -> resp (Wai.responseLBS HTTP.Status.status500 [] "Failed to send songs to database")
        Just files -> do
          result' <- sendTx (newAlbumTx songNames files)
          case result' of
            Nothing ->  resp (Wai.responseLBS HTTP.Status.status500 [] "Failed to add songs to new album")
            (Just ()) -> resp (Wai.responseLBS HTTP.Status.status200 [] "")

main :: IO ()
main = do
  Warp.run
    2719
    (\req resp ->
      case (Wai.requestMethod req, Wai.pathInfo req) of
        ("GET", [url]) -> runYtdlp (T.unpack url) resp
        _ -> resp (Wai.responseLBS HTTP.Status.status404 [] "404 Not found")
    )
