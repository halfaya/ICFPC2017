{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Data where

import GHC.Generics
import Data.Aeson.Encode.Pretty
import Data.Aeson
import Data.Aeson.Types
import qualified Data.HashMap.Strict as H

data River = River {
  source :: Int,
  target :: Int
  } deriving (Generic, Show)
instance FromJSON River
instance ToJSON River where toEncoding = genericToEncoding defaultOptions

data Site = Site {
  id :: Int,
  x  :: Double,
  y  :: Double
  } deriving (Generic, Show)
instance FromJSON Site
instance ToJSON Site where toEncoding = genericToEncoding defaultOptions

data Map = Map {
  sites  :: [Site],
  rivers :: [River],
  mines  :: [Int]
  } deriving (Generic, Show)
instance FromJSON Map
instance ToJSON Map where toEncoding = genericToEncoding defaultOptions

data InitialState = InitialState {
  punter  :: Int,
  punters :: Int,
  map     :: Map
  } deriving (Generic, Show)
instance FromJSON InitialState
instance ToJSON InitialState where toEncoding = genericToEncoding defaultOptions

data ServerMessage = Move ServerMove | Stop ServerStop
  deriving Show

instance FromJSON ServerMessage where
  parseJSON = withObject "ServerMessage" $ \obj -> case H.lookup "move" obj of
    Just (Object v)  -> (Move . ServerMove) <$> (v .: "moves")
    Nothing -> case H.lookup "stop" obj of
      Just (Object v) -> Stop <$> (ServerStop
                                    <$> (v .: "moves")
                                    <*> (v .: "scores"))
      Nothing -> fail $ "ServerMessage JSON object neither move nor stop"

data Move = Claim ClaimMove | Pass PassMove
  deriving Show

instance FromJSON Move where
  parseJSON = withObject "Move" $ \obj -> case H.lookup "pass" obj of
    Just (Object v)  -> (Pass . PassMove) <$> (v .: "punter")
    Nothing -> case H.lookup "claim" obj of
      Just (Object v) -> Claim <$> (ClaimMove
                                    <$> (v .: "punter")
                                    <*> (v .: "source")
                                    <*> (v .: "target"))
      Nothing -> fail $ "Move JSON object neither pass nor claim: " ++ show obj

instance ToJSON Move where
  toJSON (Claim (ClaimMove p s t)) = object ["claim" .= object ["punter" .= p, "source" .= s, "target" .= t]]
  toJSON (Pass  (PassMove  p))     = object ["pass"  .= object ["punter" .= p]]

data ClaimMove = ClaimMove {
  mpunter :: Int,
  msource :: Int,
  mtarget :: Int
  } deriving (Show)

data PassMove = PassMove {
  ppunter :: Int
  } deriving (Show)

data Moves = Moves {
  moves :: Move
  } deriving (Generic, Show)
instance FromJSON Moves
instance ToJSON Moves where toEncoding = genericToEncoding defaultOptions

data ServerMove = ServerMove {
  move :: [Move]
  } deriving (Generic, Show)
instance FromJSON ServerMove
instance ToJSON ServerMove where toEncoding = genericToEncoding defaultOptions

data Score = Score {
  spunter :: Int,
  sscore  :: Int
  } deriving (Generic, Show)
instance FromJSON Score where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = drop 1 }

data ServerStop = ServerStop {
  smoves  :: [Move],
  sscores :: [Score]
  } deriving (Generic, Show)
instance FromJSON ServerStop where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = drop 1 }

-------

