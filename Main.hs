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
      el "h2" $ text $ "Facts (as of May 7, 2021)"
      el "ul" $ do
        el "li" $ text $ "Tezos FA2 Token"
        el "li" $ text $ "651k total supply (no more will be generated, ever)"
        el "li" $ text $ "Token of hicetnunc.xyz (~20k xtz 24h volume)"

        el "li" $ text $ "~15 million market cap"
        el "li" $ text $ "Largest holder < 3% of total supply"
        el "li" $ text $ "Not yet listed on any exchange, can only be bought if you hold Tezos outside an exchange."

      el "h2" $ text $ "Utility"
      el "ul" $ do
        el "li" $ text $ "OBJK (nft) upvoting"
        el "li" $ text $ "hicetnunc governance (in development, to be released after Florence Tezos update lands)"
        el "li" $ text $ "Decision on hicetnunc dividends (after governance lands)"
     
      -- Adapted from work by Leonard Schoelch. Hey Leo, thanks!
      el "h2" $ text $ "Useful links"
     
      el "ul" $ do
        el "li" $ elAttr "a" ("href" =: "https://github.com/hicetnunc2000/hicetnunc/wiki/hDAO" )  (text "hDAO Wiki Page (includes instructions on how to buy")
 
        el "li" $ elAttr "a" ("href" =: "https://hicetnunc.xyz" )  (text "hic et nunc")
        el "li" $ elAttr "a" ("href" =: "https://tzkt.io/KT1QxLqukyfohPV5kPkw97Rs6cw1DDDvYgbB/dex" )  (text "Recent price chart")
        el "li" $ elAttr "a" ("href" =: "https://objkts.xyz" )  (text "current price and pool volume")
        el "li" $ elAttr "a" ("href" =: "https://app.powerbi.com/view?r=eyJrIjoiZTdmMzkxOWUtNTNlMC00ZjY2LWFiMmMtZWNmMTI5NWUyYTJlIiwidCI6IjUxM2JkYzVlLTFjMTQtNDFiYS1iZmEwLWU4NmU2MjEzZDI4ZiIsImMiOjl9" )  (text "hic et nunc sales volume")
        el "li" $ elAttr "a" ("href" =: "https://hicetnunc.xyz/objkt/60123" )  (text "Top hDAO holders")
        el "li" $ elAttr "a" ("href" =: "https://www.hicetnunc.xyz/tz/tz1c2iwyckUCcicx2qxqtwLEartYFEHg1pvB" )  (text "hDAO live information as NFTs, how meta.")

      el "h2" $ text $ "Technical Info"
      el "p" $ text $ "hDAO contract address KT1AFA2mwNUMNd4SsujE1YYp29vd8BZejyKW token id 0"
      el "div" $ do
      return ()
  }

backend :: O.Backend BackendRoute FrontendRoute
backend = O.Backend
  { O._backend_run = \serve -> serve $ const $ return ()
  , O._backend_routeEncoder = fullRouteEncoder
  }

main = O.run 8001 (O.runServeAsset "static")  backend frontend
