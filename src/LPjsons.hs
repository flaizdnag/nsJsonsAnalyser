{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module LPjsons (
    LPparams (..),
    ClausesData (..),
    AtomsData (..),
    LPafter (..),
) where

import Data.Aeson
import GHC.Generics
import System.IO

import JsonHandling as JH
import LogicPrograms as LP
import NNdecoder
import NeuralNetworks

data LPparams = LPparams
    { clauses :: ClausesData
    , facts :: Int
    , assumptions :: Int
    }
    deriving (Show, Read, Generic)

instance FromJSON LPparams
instance ToJSON LPparams where
    toEncoding = genericToEncoding defaultOptions

data ClausesData = ClausesData
    { amount :: Int
    , onlyPos :: Int
    , onlyNeg :: Int
    , mix :: Int
    , headWithH :: Int
    , atoms :: AtomsData
    , difAtoms :: AtomsData
    , atomsInHeadsNotBodies :: [Atom]
    , atomsInBodiesNotInHeads :: [Atom]
    , numOfPosAtomsInEachClause :: [Int]
    , numOfNegAtomsInEachClause :: [Int]
    , numOfClausesWhoseHeadAppearsInTheBody :: Int
    , numOfClausesWhoseHeadAppearsInABody :: Int
    }
    deriving (Show, Read, Generic)

instance FromJSON ClausesData
instance ToJSON ClausesData where
    toEncoding = genericToEncoding defaultOptions

data AtomsData = AtomsData
    { sum :: Int
    , pos :: Int
    , neg :: Int
    , withH :: Int
    , posWithH :: Int
    , negWithH :: Int
    }
    deriving (Show, Read, Generic)

instance FromJSON AtomsData
instance ToJSON AtomsData where
    toEncoding = genericToEncoding defaultOptions

newtype LPafter = LPafter
    { lp :: LPjson
    }
    deriving (Show, Read, Eq, Generic)

instance FromJSON LPafter
instance ToJSON LPafter where
    toEncoding = genericToEncoding defaultOptions
