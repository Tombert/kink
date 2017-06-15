{-# LANGUAGE OverloadedStrings #-}
module Lib where
import Network.Wai
import Data.Conduit.Process as DCP
import Data.Conduit.Binary as CL
import Data.Conduit.List as CLS
import Data.HashMap.Lazy as HML
import qualified Data.Text as T
import Data.Maybe as DM
import Conduit
import Network.HTTP.Types
import Data.ByteString as BS 
import Data.ByteString.Builder as BSB
import Data.Binary.Builder as BSB
import Network.Wai.Handler.Warp (run)
import Network.Wai.Conduit
import GHC.Word

callback :: Builder -> Flush Builder 
callback x = Chunk x
-- -> HashMap BS.ByteString BS.ByteString 
transcode ::  (Response -> IO ResponseReceived) -> HashMap BS.ByteString BS.ByteString -> IO ResponseReceived
transcode respond queryParams = do
    -- let startTime' = HML.lookup "start" queryParams 
    --     startTime = fromMaybe startTime'
    Prelude.putStrLn "I've done some IO here"
    (Inherited, src, Inherited, cph) <-
        DCP.streamingProcess (shell "ffmpeg -i /home/tombert/Downloads/sintel.mp4 -vcodec libx264 -x264-params keyint=60:no-scenecut  -f matroska -")
        --DCP.streamingProcess (shell "cat /home/tombert/Downloads/sintel.mp4")
                 -- .| mapC BS.concat

    respond $ responseSource status200 [("Content-Type", "video/webm") ,("X-Content-Duration", "90.0")] (src .| mapC BSB.fromByteString .| mapC Chunk) 


filterOptions =
    DM.mapMaybe (\(key, value)->
        case value of
            Just y -> Just (key, y)
            Nothing -> Nothing
                            )
extractQueryParams = (fromList . filterOptions . queryString )

appRouter :: Application
appRouter request respond = 
      let fart = queryParams in 
      case rawPathInfo request of
          "/transcode" -> transcode respond fart

    where
        pathparts = pathInfo request
        queryParams =  extractQueryParams request

someFunc :: IO ()
someFunc = do
    Prelude.putStrLn $ "http://localhost:8080/"
    run 8080 appRouter 
