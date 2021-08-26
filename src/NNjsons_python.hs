{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module NNjsons_python
    ( NNatoms (..)
    , NNpython (..)
    , NNarchitecture (..)
    ) where


import           Data.Aeson
import           GHC.Generics
import           System.IO


import           JsonHandling    as JH
import           LogicPrograms   as LP
import           NNdecoder
import           NNjsons_python1 as NNpy1
import           NeuralNetworks


data NNatoms = NNatoms
    { sum :: Int
    , inp :: Int
    , hid :: Int
    , out :: Int
    , rec :: Int
    } deriving (Show, Read, Generic)

instance FromJSON NNatoms
instance ToJSON NNatoms where
    toEncoding = genericToEncoding defaultOptions


data NNpython = NNpython
    { architecture    :: NNarchitecture
    , i2h_connections :: [[Float]]
    , h2o_connections :: [[Float]]
    , o2i_connections :: [[Float]]
    , factors         :: Factors
    , comments        :: [String]
    } deriving (Show, Read, Generic)

instance FromJSON NNpython
instance ToJSON NNpython where
    toEncoding = genericToEncoding defaultOptions


data NNarchitecture = NNarchitecture
    { inpLayer            :: [Neuron]
    , hidLayer            :: [Neuron]
    , outLayer            :: [Neuron]
    , recLayer            :: [Neuron]
    , inpToHidConnections :: [Connection]
    , hidToOutConnections :: [Connection]
    , recConnections      :: [Connection]
    } deriving (Show, Read, Generic)

instance FromJSON NNarchitecture
instance ToJSON NNarchitecture where
    toEncoding = genericToEncoding defaultOptions

