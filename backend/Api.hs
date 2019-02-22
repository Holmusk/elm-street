{-# LANGUAGE DataKinds #-}

module Api
       ( app
       ) where

import Control.Monad.IO.Class (liftIO)
import Data.Proxy (Proxy (..))
import Network.Wai (Application)
import Servant.API ((:<|>) (..), (:>), Get, JSON, Post, ReqBody)
import Servant.Server (Handler, Server, serve)

import Types (OneType, defaultOneType)


type TypesApi
    -- | Get 'OneType' from the backend side
       = "get" :> Get '[JSON] OneType
    -- | Receive 'OneType' from the frontend side
    :<|> "post" :> ReqBody '[JSON] OneType :> Post '[JSON] Bool

typesApi :: Proxy TypesApi
typesApi = Proxy

typesServer :: Server TypesApi
typesServer = getHandler :<|> postHandler

getHandler :: Handler OneType
getHandler = liftIO $ putStrLn "Get handler" >> pure defaultOneType

postHandler :: OneType -> Handler Bool
postHandler = liftIO . (putStrLn "Post handler" >>) . pure . (defaultOneType ==)

app :: Application
app = serve typesApi typesServer
