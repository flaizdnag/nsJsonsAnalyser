{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module NNjsons 
    ( NNparams (..)
    , NNconnections (..)
    ) where


import           Data.Aeson
import           GHC.Generics
import           System.IO


import           JsonHandling    as JH
import           LogicPrograms   as LP
import           NNdecoder
import           NNjsons_python  as NNpy
import           NNjsons_python1 as NNpy1
import           NeuralNetworks


data NNparams = NNparams
    { atoms       :: NNatoms
    , connections :: NNconnections
    , bigWeights  :: NNbigWeights
    , factors     :: Factors
    } deriving (Show, Read, Generic)

instance FromJSON NNparams
instance ToJSON NNparams where
    toEncoding = genericToEncoding defaultOptions


data NNconnections = NNconnections
    { inp2Hid :: Int
    , hid2Out :: Int
    , rec     :: Int
    } deriving (Show, Read, Generic)

instance FromJSON NNconnections
instance ToJSON NNconnections where
    toEncoding = genericToEncoding defaultOptions
