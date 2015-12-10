{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Lib
    ( startApp
    ) where

import Control.Monad.Trans.Either
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant



data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show)

type Handler = EitherT ServantErr IO
type API
  =   "users" :> Get '[JSON] [User]
  :<|> "firstUser" :> Get '[JSON] User

users :: Handler [User]
users =
  return
    [ User 1 "Isaac" "Newton"
    , User 2 "Albert" "Einstein"
    ]

firstUser :: Handler User
firstUser = head <$> users



$(deriveJSON defaultOptions ''User)

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server
  = users
  :<|> firstUser


