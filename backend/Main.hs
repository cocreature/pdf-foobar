{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

import           Control.Monad.IO.Class
import           Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as ByteString (fromStrict)
import           Data.List
import qualified Data.Text as Text
import qualified Network.HTTP.Media as Media
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.Multipart
import           Servant.Server
import           System.IO.Temp
import qualified System.Process as Process
import           Text.Read

data ExtractPages = ExtractPages
  { expgsFilePath :: FilePath
  , expgsPages :: [Word]
  }

instance FromMultipart ExtractPages where
  fromMultipart multipartData =
    ExtractPages <$> fmap fdFilePath (lookupFile "file" multipartData) <*>
    (readMaybe =<< (fmap Text.unpack (lookupInput "pages" multipartData)))

data PDF

instance Accept PDF where
  contentType _ = "application" Media.// "pdf"

instance MimeRender PDF ByteString where
  mimeRender _ bs = ByteString.fromStrict bs

type API = MultipartForm ExtractPages :> Post '[PDF] ByteString

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

server :: ExtractPages -> Handler ByteString
server ep = liftIO (extractPages ep)

app :: Application
app = serve (Proxy :: Proxy API) server

main :: IO ()
main = do
  run 3000 app
