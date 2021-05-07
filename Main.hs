{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}
module Main where

import qualified Control.Monad
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Text.Lazy (toStrict)
import Reflex.Dom.Core (el, text, elAttr, (=:))
import Data.Functor.Identity (Identity)

import Obelisk.Route ( pattern (:/) )

import qualified Obelisk.Frontend as O
import qualified Obelisk.Backend as O
import qualified Obelisk.Route.TH as O
import qualified Obelisk.Run as O

import qualified Obelisk.Route as O

import Clay
import qualified Clay.Font as Font

data BackendRoute :: * -> * where
  BackendRoute_Missing :: BackendRoute ()

data FrontendRoute :: * -> * where
  FrontendRoute_Main :: FrontendRoute ()

fullRouteEncoder
  :: O.Encoder (Either T.Text) Identity (O.R (O.FullRoute BackendRoute FrontendRoute)) O.PageName
fullRouteEncoder = O.mkFullRouteEncoder
  (O.FullRoute_Backend BackendRoute_Missing :/ ())
  (\case
      BackendRoute_Missing -> O.PathSegment "missing" $ O.unitEncoder mempty)
  (\case
      FrontendRoute_Main -> O.PathEnd $ O.unitEncoder mempty)

concat <$> mapM O.deriveRouteComponent
  [ ''BackendRoute
  , ''FrontendRoute
  ]

commonTitle :: String
commonTitle = "hDAO - hic et nunc DAO"

myStylesheet :: Css
myStylesheet = body ? 
                  do background  white
                     color       black
                     fontFamily  ["Courier New", "Courier"] [monospace]

frontend :: O.Frontend (O.R FrontendRoute)
frontend = O.Frontend
  { O._frontend_head = do
      el "title" $ text $ T.pack commonTitle
      el "style" $ text $ toStrict $ render $ myStylesheet
  , O._frontend_body = do
      el "h1" $ text $ T.pack commonTitle

      el "h2" $ text $ "What is hDAO?"
      el "p" $ text "hDAO is the governance token for the decentralized digital assets marketplace hic et nunc"
      el "h2" $ text $ "Useful links"
     
      el "ul" $ do
        el "li" $  elAttr "a"
        ("href" =: "https://github.com/hicetnunc2000/hicetnunc/wiki/hDAO" )  (text "hDAO Wiki Page")
 
        el "li" $  elAttr "a"
        ("href" =: "https://hicetnunc.xyz" )  (text "hic et nunc")

      el "div" $ do
      return ()
  }

backend :: O.Backend BackendRoute FrontendRoute
backend = O.Backend
  { O._backend_run = \serve -> serve $ const $ return ()
  , O._backend_routeEncoder = fullRouteEncoder
  }


main = O.run 8001 (O.runServeAsset "static")  backend frontend
