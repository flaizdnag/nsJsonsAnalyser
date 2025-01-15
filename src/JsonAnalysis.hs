{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : JsonAnalysis
Description : Jsons with full information (lp -> nn -> lp) analysis.
Copyright   : (c) Andrzej G., 2021-
License     : GPL-3
Maintainer  : andrzej.m.gajda@gmail.com
Stability   : experimental
Portability : POSIX

Functions that allow to analyse jsons with results obtained from experiments,
where lp is translated to nn, nn is trained, and nn is translated back to lp.
-}
module JsonAnalysis (
    JsonToAnalyse (..),
    analyseJsons,
) where

import Data.Aeson
import Data.List
import Data.ByteString.Lazy.UTF8 as BS
import qualified Data.Map as Map
import GHC.Generics
import System.Directory
import System.FilePath.Posix
import System.IO

import JsonHandling as JH
import LPjsons
import LogicPrograms as LP
import NNdecoder
import NNjsons
import NNjsons_python as NNpy
import NNjsons_python1 as NNpy1
import NeuralNetworks as NN

data JsonToAnalyse = JsonToAnalyse
    { lp_before :: LPjson
    , lp_before_params :: LPparams
    , neural_network_factors :: Factors
    , nn_recipe :: NNwithAmin
    , nn_before :: NNpython
    , nn_after :: NNpython
    , errors :: [Float]
    , io_pairs :: [[[Float]]]
    , lp_after :: LPafter
    , lp_after_params :: LPparams
    }
    deriving (Show, Read, Generic)

instance FromJSON JsonToAnalyse
instance ToJSON JsonToAnalyse where
    toEncoding = genericToEncoding defaultOptions

analyseJsons :: IO ()
analyseJsons = do
    let checkedLPsFile = "logic_programs.json"
    writeFile checkedLPsFile ""
    putStrLn $ "Created file " ++ checkedLPsFile ++ " with logic programs"
    putStrLn $ "Writing logic programs to " ++ checkedLPsFile

    files <- listDirectory "results"

    lps <- unlines . map writeResults <$> recAnalyser files (return [])
    appendFile checkedLPsFile lps

    putStrLn "Done"


data WeightFullInfo = WeightFullInfo
  { minWeight :: Float
  , maxWeight :: Float
  , sumWeight :: Float
  , avgWeight :: Float
  , numWeight :: Int
  } deriving (Show, Read, Generic)

instance FromJSON WeightFullInfo
instance ToJSON WeightFullInfo where
    toEncoding = genericToEncoding defaultOptions


data LPData = LPData
  { lp :: LPjson
  , lpQuantity :: Int
  , i2h :: [[WeightFullInfo]]
  , h2o :: [[WeightFullInfo]]
  , inpLayout :: [Neuron]
  , hidLayout :: [Neuron]
  , outLayout :: [Neuron]
  } deriving (Show, Read, Generic)

instance FromJSON LPData
instance ToJSON LPData where
    toEncoding = genericToEncoding defaultOptions


combineWeights :: WeightFullInfo -> WeightFullInfo -> WeightFullInfo
combineWeights (WeightFullInfo aMin aMax aSum aAvg aNum) (WeightFullInfo bMin bMax bSum bAvg bNum) = combined
  where
    combined = WeightFullInfo newMin newMax newSum newAvg newNum
    newMin = min aMin bMin
    newMax = min aMax bMax
    newSum = aSum + bSum
    newNum = aNum + bNum
    newAvg = newSum / (fromIntegral newNum :: Float)

weightToFullWeightInfo :: Float -> WeightFullInfo
weightToFullWeightInfo x = WeightFullInfo x x x x 1

printFullInfoWeights :: [[WeightFullInfo]] -> String
printFullInfoWeights ws = intercalate "\n\n" (map showWs ws)
  where
    showWs :: [WeightFullInfo] -> String
    showWs xs = intercalate "\n" (map show xs)

-- [[1, 2, 3], [4, 5, 6]]
--
--
-- [[WeightFullInfo 1 1 1, WeightFullInfo 2 2 2, WeightFullInfo 3 3 3], [WeightFullInfo 4 4 4, WeightFullInfo 5 5 5, WeightFullInfo 6 6 6]]

recAnalyser :: [FilePath] -> IO [(LPjson, LPData)] -> IO [(LPjson, LPData)]
recAnalyser [] accIO = do
  -- calculate average
  accIO
recAnalyser (file : files) accIO = recAnalyser files newAcc
  where
    newAcc = do
      acc <- accIO
      (lp, i2h_new, h2o_new, inp_layout, hid_layout, out_layout) <- lpIO

      let accMap = Map.fromList acc

      case Map.lookup lp accMap of
        Nothing -> do
          let
            i2hFullInfo = map (map weightToFullWeightInfo) i2h_new
            h2oFullInfo = map (map weightToFullWeightInfo) h2o_new

          return . Map.toList $ Map.insert lp (LPData lp 1 i2hFullInfo h2oFullInfo inp_layout hid_layout out_layout) accMap
        Just (LPData _ val i2h_old h2o_old _ _ _) -> do
          let
            i2hFullInfo = map (map weightToFullWeightInfo) i2h_new
            h2oFullInfo = map (map weightToFullWeightInfo) h2o_new

            addAllWeights :: [[WeightFullInfo]] -> [[WeightFullInfo]] -> [[WeightFullInfo]]
            addAllWeights = summed
              where
                summed xs ys = map (\(x, y) -> zipWith combineWeights x y) (zip xs ys)

          return . Map.toList $ Map.insert lp (LPData lp (val + 1) (addAllWeights i2hFullInfo i2h_old) (addAllWeights h2oFullInfo h2o_old) inp_layout hid_layout out_layout) accMap

    lpIO = do
      json <- decodeFileStrict ("results/" ++ file) :: IO (Maybe JsonToAnalyse)
      case json of
        -- Nothing -> error "Could not process file " ++ file
        Nothing -> undefined
        Just xs -> do
          let 
            lp_to_save = LPjsons.lp $ lp_after xs
            i2h_to_save = i2h_connections $ nn_after xs
            h2o_to_save = h2o_connections $ nn_after xs
            inp_layout = NNpy.inpLayer . architecture $ nn_after xs
            hid_layout = NNpy.hidLayer . architecture $ nn_after xs
            out_layout = NNpy.outLayer . architecture $ nn_after xs

          return (lp_to_save, i2h_to_save, h2o_to_save, inp_layout, hid_layout, out_layout)


writeResults :: (LPjson, LPData) -> String
writeResults (_, lpData) = BS.toString $ encode lpData
