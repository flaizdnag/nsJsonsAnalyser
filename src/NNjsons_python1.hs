{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module NNjsons_python1
    ( NNbigWeights (..)
    ) where


import           Data.Aeson
import           GHC.Generics
import           System.IO


import           JsonHandling    as JH
import           LogicPrograms   as LP
import           NNdecoder
import           NeuralNetworks


data NNbigWeights = NNbigWeights
    { inp2Hid :: Int
    , hid2Out :: Int
    , rec     :: Int
    } deriving (Show, Read, Generic)

instance FromJSON NNbigWeights
instance ToJSON NNbigWeights where
    toEncoding = genericToEncoding defaultOptions
