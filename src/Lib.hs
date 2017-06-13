{-# LANGUAGE OverloadedStrings #-}
module Lib where
import Network.Wai
import Data.Conduit.Process as DCP
import Data.Conduit.Binary as CL
import Data.Conduit.List as CLS
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

app :: Application
app _ respond = do
    Prelude.putStrLn "I've done some IO here"
    (Inherited, src, Inherited, cph) <-
        DCP.streamingProcess (shell "ffmpeg -i /home/tombert/Downloads/sintel.mp4 -f matroska -")
                 -- .| mapC BS.concat

    let fart = src .| mapC BSB.fromByteString .| mapC Chunk
    respond $ responseSource status200 [] fart 

someFunc :: IO ()
someFunc = do
    Prelude.putStrLn $ "http://localhost:8080/"
    run 8080 app
