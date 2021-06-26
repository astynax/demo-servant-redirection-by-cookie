{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Proxy
import Data.Text
import Data.Text.Encoding as TE
import Network.Wai.Handler.Warp
import Servant.API
import Servant.Server
import Web.Cookie

type Api = Index :<|> Login

type Index =
  Header "Cookie" Cookies'
  :> UVerb 'GET '[PlainText]
  '[ WithStatus 200 Text
  ,  WithStatus 303 (Headers '[Header "Location" Text] Text)
  ]

type Login =
  "login" :> Verb 'GET 303 '[PlainText]
    (Headers
     '[ Header "Location" Text
     ,  Header "Set-Cookie" SetCookie
     ] Text
     )

newtype Cookies' = Cookies' { unCookies' :: Cookies }

instance FromHttpApiData Cookies' where
  parseHeader = return . Cookies' . parseCookies
  parseQueryParam = return . Cookies' . parseCookies . TE.encodeUtf8

server :: Server Api
server = index :<|> login
  where
    index :: Maybe Cookies' -> Handler
      (Union
      '[ WithStatus 200 Text
      ,  WithStatus 303 (Headers '[Header "Location" Text] Text)
      ])
    index cookies =
      case cookies >>= lookup "logged_in" . unCookies' of
        Just "true" ->
          respond
            $ WithStatus @200 @Text
            $ "Yay!"
        _ ->
          respond
            $ WithStatus @303
            $ addHeader @"Location" @Text @Text "/login"
            $ "ok"
    login = do
      let cookie = defaultSetCookie
            { setCookieName = "logged_in"
            , setCookieValue = "true"
            }
      pure
        $ addHeader @"Location" @Text "/"
        $ addHeader @"Set-Cookie" @SetCookie cookie
        $ "ok"

app :: Application
app = serve (Proxy @Api) server

main :: IO ()
main = Network.Wai.Handler.Warp.run 8000 app
