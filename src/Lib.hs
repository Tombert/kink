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
transcode ::  (Response -> IO ResponseReceived) -> T.Text -> HashMap BS.ByteString BS.ByteString -> IO ResponseReceived
transcode respond path queryParams = do
    -- let startTime' = HML.lookup "start" queryParams 
    --     startTime = fromMaybe startTime'
    Prelude.putStrLn "I've done some IO here"
    --(Inherited, src, Inherited, cph) <-
        --DCP.streamingProcess (shell "ffmpeg -i /home/tombert/Downloads/sintel.mp4  -f matroska /home/tombert/fart/fart.3u8")
     --   DCP.streamingProcess (shell ("cat /home/tombert/Downloads/" ++ (T.unpack path )))
                 -- .| mapC BS.concat

    --respond $ responseSource status200 [("X-Content-Duration", "90.0")] (src .| mapC BSB.fromByteString .| mapC Chunk) 
    
    respond $ responseFile status200 [("Content-Type", "video/mp4"), ("Content-Duration", "90.0")] ("/home/tombert/Downloads/" ++ (T.unpack path )) Nothing


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
      case pathparts of
          ["transcode", x] -> transcode respond x fart

    where
        pathparts = pathInfo request
        queryParams =  extractQueryParams request

someFunc :: IO ()
someFunc = do
    Prelude.putStrLn $ "http://localhost:8080/"
    run 8080 appRouter 
