module Main (main) where

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
newAlbumTx :: [LBS.ByteString] -> LBS.ByteString
newAlbumTx files =
  LBS.intercalate "\n" $ List.concat
    [ ["("]
    , [LBS.intercalate " " ["(", "album", "\"is\"", "\"album\"", "#t", ")"]]
    , [LBS.intercalate " " ["(", "album", "\"name\"", "\"New Album\"", "#t", ")"]]
    , List.concat (zipWith (newSongTxElem "album") [1..] files)
    , [")"]
    ]

newSongTxElem :: LBS.ByteString -> Int -> LBS.ByteString -> [LBS.ByteString]
newSongTxElem album n file =
  [ LBS.intercalate " " ["(", album, "\"includes track\"", track, "#t", ")"]
  , LBS.intercalate " " ["(", track, "\"is\"", "\"track\"", "#t", ")"]
  , LBS.intercalate " " ["(", track, "\"number\"", fromString (show n), "#t", ")"]
  , LBS.intercalate " " ["(", track, "\"song\"", song, "#t", ")"]
  , LBS.intercalate " " ["(", song, "\"is\"", "\"song\"", "#t", ")"]
  , LBS.intercalate " " ["(", song, "\"name\"", LBS.concat ["\"Song ", fromString (show n), "\""], "#t", ")"]
  , LBS.intercalate " " ["(", song, "\"file\"", file, "#t", ")"]
  ]
  where
    track = LBS.append "track" (fromString (show n))
    song = LBS.append "song" (fromString (show n))

runYtdlp :: String -> (Wai.Response -> IO Wai.ResponseReceived) -> IO Wai.ResponseReceived
runYtdlp url resp = withSystemTempDirectory "qstore-ytdlp" \tmpDirPath -> do
  exitCode <- runProcess (fromString (printf "yt-dlp --no-playlist -x --force-overwrite --audio-format mp3 -o '%s/%%(title)s.%%(ext)s' '%s'" tmpDirPath url))
  case exitCode of
    ExitFailure _ -> resp (Wai.responseLBS HTTP.Status.status400 [] "Couldn't download link")
    ExitSuccess -> do
      songNames <- listDirectory tmpDirPath
      let songPaths = ((tmpDirPath ++ "/") ++) <$> songNames
      result <- sequence <$> sequence (map sendFile songPaths)
      case result of
        Nothing -> resp (Wai.responseLBS HTTP.Status.status500 [] "Failed to send songs to database")
        Just files -> do
          result' <- sendTx (newAlbumTx files)
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
