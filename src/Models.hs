{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Models where

import           Data.Aeson          (FromJSON, Object, ToJSON, object,
                                      parseJSON, toJSON, withObject, (.:), (.=))
import           Data.Aeson.Types    (Pair, Parser)
import           Data.Text
import           Data.Time

import           Database.Persist.TH

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
  Sister
    name Text
    UniqueName name
    password Text
    deriving Read Show
  Note
    sender Text
    text Text
    time UTCTime default=CURRENT_TIME
    deriving Read Show
|]

instance ToJSON Sister where
  toJSON sister =
    object
      [ "name" .= toJSON (sisterName sister)
      , "password" .= toJSON (sisterPassword sister)
      ]

instance FromJSON Sister where
  parseJSON = withObject "Sister" parseSister

parseSister :: Object -> Parser Sister
parseSister o = do
  sname <- o .: "name"
  spassword <- o .: "password"
  return Sister {sisterName = sname, sisterPassword = spassword}

instance ToJSON Note where
  toJSON note =
    object
      [ "sender" .= toJSON (noteSender note)
      , "time" .= toJSON (noteTime note)
      , "text" .= toJSON (noteText note)
      ]

instance FromJSON Note where
  parseJSON = withObject "Note" parseNote

parseNote :: Object -> Parser Note
parseNote o = do
  nsender <- o .: "sender"
  ntime <- o .: "time"
  ntext <- o .: "text"
  return Note {noteSender = nsender, noteTime = ntime, noteText = ntext}
