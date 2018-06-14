{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

import           Control.Monad.IO.Class
import           Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as ByteString (fromStrict)
import           Data.List
import           Data.Monoid
import qualified Data.Text as Text
import qualified Network.HTTP.Media as Media
import           Network.Wai.Handler.Warp
import           Options.Applicative
import           Servant
import           Servant.Multipart
import           Servant.Server
import           Servant.Utils.StaticFiles
import           System.IO.Temp
import qualified System.Process as Process
import           Text.Read

data Options = Options
  { staticPath :: !FilePath
  , port :: !Int
  } deriving (Show, Eq, Ord)

optParser :: Parser Options
optParser =
  Options <$>
  strOption
    (short 'd' <> metavar "DIRECTORY" <>
     help "Path to the directory containing static files") <*>
  option
    auto
    (short 'p' <> metavar "PORT" <> help "Port that the server should listen on" <>
     value 3000)

data ExtractPages = ExtractPages
  { expgsFilePath :: FilePath
  , expgsPages :: [Word]
  }

instance FromMultipart Tmp ExtractPages where
  fromMultipart multipartData =
    ExtractPages <$> fmap fdPayload (lookupFile "file" multipartData) <*>
    (readMaybe =<< (fmap Text.unpack (lookupInput "pages" multipartData)))

data PDF

instance Accept PDF where
  contentType _ = "application" Media.// "pdf"

instance MimeRender PDF ByteString where
  mimeRender _ bs = ByteString.fromStrict bs

type API
   = "api" :> MultipartForm Tmp ExtractPages :> Post '[ PDF] ByteString :<|> Raw

extractPages :: ExtractPages -> IO ByteString
extractPages (ExtractPages filePath pages) = do
  withSystemTempFile "out.pdf" $ \outFilePath _handle -> do
    -- TODO handle failures
    _ <-
      Process.readProcess
        "mutool"
        ["merge", "-o", outFilePath, filePath, intercalate "," (map show pages)]
        []
    ByteString.readFile outFilePath

apiHandler :: ExtractPages -> Handler ByteString
apiHandler ep = liftIO (extractPages ep)

server :: FilePath -> Server API
server staticPath = apiHandler :<|> serveDirectoryFileServer staticPath

app :: FilePath -> Application
app staticPath = serve (Proxy :: Proxy API) (server staticPath)

main :: IO ()
main = do
  Options staticPath port <- execParser optInfo
  run port (app staticPath)
  where
    optInfo = info (optParser <**> helper) (fullDesc)
