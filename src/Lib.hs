{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Lib
    ( someFunc
    ) where
import Network.HTTP.Req
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Aeson.Types
import Data.Text (Text)
import qualified Data.Text.IO as DTI (putStrLn)

-- JSONを受取り値を取り出すサンプル
-- see http://note.kurodigi.com/haskell-req/
someFunc :: IO ()
someFunc = do
  runReq defaultHttpConfig request
  where
  -- 通信処理
  request :: Req ()
  request = do
    res <- req GET
               (https "httpbin.org" /: "ip")
               NoReqBody
               jsonResponse
               mempty
    let val = (responseBody res :: Value)
    case val of
      Object o ->
        case parse getIp o of
          Success r -> liftIO $ DTI.putStrLn r
          Error e -> liftIO $ Prelude.putStrLn e

  getIp :: Object -> Parser Text
  getIp o = do o .: "origin"
