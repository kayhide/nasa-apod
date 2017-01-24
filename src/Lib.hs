{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib
    ( someFunc
    ) where

import Safe
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Lens
import Control.Monad
import GHC.Generics
import Data.Aeson
import Reflex.Dom

data NasaPicture = NasaPicture
                 { copyright :: String
                 , date :: String
                 , explanation :: String
                 , hdurl :: String
                 , media_type :: String
                 , service_version :: String
                 , title :: String
                 , url :: String
                 } deriving (Show, Generic)

instance FromJSON NasaPicture

someFunc :: IO ()
someFunc = mainWidget $ do
  el "div" apodClient
  return ()

apodClient :: forall t m. MonadWidget t m => m ()
apodClient = do
  el "h1" $ text "NASA Astronomy Picture of the Day"
  t <- textInput $ def & textInputConfig_initialValue .~ "DEMO_KEY"
       & textInputConfig_attributes .~ constDyn (Map.fromList [("size", "60")])
  let apiKey = t ^. textInput_value
  b <- button "Send Request"
  let buttonEvent = tagDyn apiKey b :: Event t String
      enterEvent = tagDyn apiKey (textInputGetEnter t)
      apiKeyEvent = leftmost [buttonEvent, enterEvent] :: Event t String
  el "div" $ do
    el "h3" $ dynText $ constDyn "API KEY"
    dynText =<< holdDyn "(Empty)" apiKeyEvent

  let req = apiKeyToXhrRequest <$> apiKeyEvent
  res <- performRequestAsync req
  let resText = _xhrResponse_responseText <$> res :: Event t (Maybe T.Text)
      resString = fmapMaybe (fmap T.unpack) resText
  el "div" $ do
    el "h3" $ text "Response"
    dynText =<< holdDyn "(Response)" resString

  let decoded = decodeXhrResponse <$> res :: Event t (Maybe NasaPicture)
  dynPic <- holdDyn Nothing decoded
  dynPicString <- mapDyn show dynPic

  imgAttrs <- mapDyn srcAttr dynPic
  elDynAttr "img" imgAttrs $ return ()
  return ()
    where srcAttr Nothing = Map.empty
          srcAttr (Just pic) = Map.singleton "src" (url pic)

apiKeyToXhrRequest :: String -> XhrRequest
apiKeyToXhrRequest k = XhrRequest { _xhrRequest_method = "GET",
                                    _xhrRequest_url = "https://api.nasa.gov/planetary/apod?api_key=" ++ k,
                                    _xhrRequest_config = def
                                  }
