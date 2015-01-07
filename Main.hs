{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Control.Monad.IO.Class
import Data.Aeson
import Data.Proxy
import Data.Text
import Data.Time
import Data.Time.Zones
import GHC.Generics
import GHC.Int (Int64)
import Network.Wai.Handler.Warp
import Servant.API
import Servant.Server
import System.Locale

             -- GET /date
type MyAPI = "date" :> Get UTCTime
             -- GET /time/:tz
        :<|> "time" :> Capture "tz" String :> Get ZonedTime

server :: Server MyAPI
server = getDate :<|> getTimeForTZ
  where
    getDate = liftIO getCurrentTime
    getTimeForTZ tzname = liftIO $ getTimeAtTZ tzname

getTimeAtTZ :: String -> IO ZonedTime
getTimeAtTZ tzname = do
    putStrLn $ "/time/" ++ tzname
    t <- getCurrentTime
    tz <- loadSystemTZ tzname
    let minutes = diffForPOSIX tz (0 :: Int64) `div` 60
    return $ ZonedTime (utcToLocalTimeTZ tz t) (minutesToTimeZone minutes)

main :: IO ()
main = do
    putStrLn "running this thing on 8000"
    Network.Wai.Handler.Warp.run 8000 $ serve myApi server

myApi :: Proxy MyAPI
myApi = Proxy
