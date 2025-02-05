{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : JsonAnalysis
-- Description : Jsons with full information (lp -> nn -> lp) analysis.
-- Copyright   : (c) Andrzej G., 2021-
-- License     : GPL-3
-- Maintainer  : andrzej.m.gajda@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Functions that allow to analyse jsons with results obtained from experiments,
-- where lp is translated to nn, nn is trained, and nn is translated back to lp.
module JsonAnalysis
  ( JsonToAnalyse (..),
    analyseJsons,
  )
where

import Data.Aeson
import Data.ByteString.Lazy.UTF8 as BS
import Data.List
import qualified Data.Map as Map
import GHC.Generics
import qualified JsonHandling as JH
import qualified LPjsons as LPj
import LogicPrograms as LP
import qualified NNdecoder as NNd
import NNjsons
import qualified NNjsons_python as NNpy
import NNjsons_python1 as NNpy1
import qualified NeuralNetworks as NN
import System.Directory
import System.FilePath.Posix
import System.IO

data JsonToAnalyse = JsonToAnalyse
  { lp_before :: JH.LPjson,
    lp_before_params :: LPj.LPparams,
    neural_network_factors :: JH.Factors,
    nn_recipe :: JH.NNwithAmin,
    nn_before :: NNpy.NNpython,
    nn_after :: NNpy.NNpython,
    errors :: [Float],
    io_pairs :: [[[Float]]],
    lp_after :: LPj.LPafter,
    lp_after_params :: LPj.LPparams
  }
  deriving (Show, Read, Generic)

instance FromJSON JsonToAnalyse

instance ToJSON JsonToAnalyse where
  toEncoding = genericToEncoding defaultOptions

analyseJsons :: IO ()
analyseJsons = do
  let checkedLPsFile = "logic_programs.jsonl"
  writeFile checkedLPsFile ""
  putStrLn $ "Created file " ++ checkedLPsFile ++ " with logic programs"
  putStrLn $ "Writing logic programs to " ++ checkedLPsFile

  files <- listDirectory "results"

  lps <- unlines . map writeResults <$> recAnalyser files (return [])
  appendFile checkedLPsFile lps

  putStrLn "Done"

data WeightFullInfo = WeightFullInfo
  { minW :: Float,
    maxW :: Float,
    sumW :: Float,
    numW :: Int
  }

data LPcounter = LPcounter
  { lpCounterLp :: JH.LPjson,
    lpCounterLpCount :: Int,
    lpCounterI2h :: [[WeightFullInfo]],
    lpCounterH2o :: [[WeightFullInfo]],
    lpCounterInpLayout :: [NN.Neuron],
    lpCounterHidLayout :: [NN.Neuron],
    lpCounterOutLayout :: [NN.Neuron]
  }

data LPdata = LPdata
  { lp :: JH.LPjson,
    lpCount :: Int,
    neuralNetwork :: AggNeuralNetwork
  }
  deriving (Show, Read, Generic)

instance FromJSON LPdata

instance ToJSON LPdata where
  toEncoding = genericToEncoding defaultOptions

data AggConnection = AggConnection
  { fromNeuron :: String,
    toNeuron :: String,
    minWeight :: Float,
    maxWeight :: Float,
    sumWeight :: Float,
    avgWeight :: Float,
    countWeights :: Int
  }
  deriving (Show, Read, Generic)

instance FromJSON AggConnection

instance ToJSON AggConnection where
  toEncoding = genericToEncoding defaultOptions

data AggNeuralNetwork = AggNeuralNetwork
  { inpLayer :: [NN.Neuron],
    hidLayer :: [NN.Neuron],
    outLayer :: [NN.Neuron],
    inpToHidConnections :: [AggConnection],
    hidToOutConnections :: [AggConnection],
    recConnections :: [AggConnection]
  }
  deriving (Show, Read, Generic)

instance FromJSON AggNeuralNetwork

instance ToJSON AggNeuralNetwork where
  toEncoding = genericToEncoding defaultOptions

combineWeights :: WeightFullInfo -> WeightFullInfo -> WeightFullInfo
combineWeights (WeightFullInfo aMin aMax aSum aNum) (WeightFullInfo bMin bMax bSum bNum) = combined
  where
    combined = WeightFullInfo newMin newMax newSum newNum
    newMin = min aMin bMin
    newMax = max aMax bMax
    newSum = aSum + bSum
    newNum = aNum + bNum

weightToFullWeightInfo :: Float -> WeightFullInfo
weightToFullWeightInfo x = WeightFullInfo x x x 1

recAnalyser :: [FilePath] -> IO [(JH.LPjson, LPcounter)] -> IO [(JH.LPjson, LPcounter)]
recAnalyser [] accIO = accIO
recAnalyser (file : files) accIO = recAnalyser files newAcc
  where
    newAcc = do
      acc <- accIO
      (lp, i2h_new, h2o_new, inp_layout, hid_layout, out_layout) <- lpIO

      let accMap = Map.fromList acc

      case Map.lookup lp accMap of
        Nothing -> do
          let i2hFullInfo = map (map weightToFullWeightInfo) i2h_new
              h2oFullInfo = map (map weightToFullWeightInfo) h2o_new

          return . Map.toList $ Map.insert lp (LPcounter lp 1 i2hFullInfo h2oFullInfo inp_layout hid_layout out_layout) accMap
        Just (LPcounter _ val i2h_old h2o_old _ _ _) -> do
          let i2hFullInfo = map (map weightToFullWeightInfo) i2h_new
              h2oFullInfo = map (map weightToFullWeightInfo) h2o_new

              addAllWeights :: [[WeightFullInfo]] -> [[WeightFullInfo]] -> [[WeightFullInfo]]
              addAllWeights = summed
                where
                  summed xs ys = map (\(x, y) -> zipWith combineWeights x y) (zip xs ys)

          return . Map.toList $ Map.insert lp (LPcounter lp (val + 1) (addAllWeights i2hFullInfo i2h_old) (addAllWeights h2oFullInfo h2o_old) inp_layout hid_layout out_layout) accMap

    lpIO = do
      json <- decodeFileStrict ("results/" ++ file) :: IO (Maybe JsonToAnalyse)
      case json of
        -- Nothing -> error "Could not process file " ++ file
        Nothing -> undefined
        Just xs -> do
          let lp_to_save = LPj.lp $ lp_after xs
              i2h_to_save = NNpy.i2h_connections $ nn_after xs
              h2o_to_save = NNpy.h2o_connections $ nn_after xs
              inp_layout = NNpy.inpLayer . NNpy.architecture $ nn_after xs
              hid_layout = NNpy.hidLayer . NNpy.architecture $ nn_after xs
              out_layout = NNpy.outLayer . NNpy.architecture $ nn_after xs

          return (lp_to_save, i2h_to_save, h2o_to_save, inp_layout, hid_layout, out_layout)

writeResults :: (JH.LPjson, LPcounter) -> String
writeResults (_, lpCounter) = BS.toString $ encode $ toLPdata lpCounter

toLPdata :: LPcounter -> LPdata
toLPdata lpCounter =
  LPdata
    { lp = lpCounterLp lpCounter,
      lpCount = lpCounterLpCount lpCounter,
      neuralNetwork =
        AggNeuralNetwork
          { inpLayer = lpCounterInpLayout lpCounter,
            hidLayer = lpCounterHidLayout lpCounter,
            outLayer = lpCounterOutLayout lpCounter,
            inpToHidConnections = makeConnections (lpCounterInpLayout lpCounter) (lpCounterHidLayout lpCounter) (lpCounterI2h lpCounter),
            hidToOutConnections = makeConnections (lpCounterHidLayout lpCounter) (lpCounterOutLayout lpCounter) (lpCounterH2o lpCounter),
            recConnections = []
          }
    }
  where
    makeConnections :: [NN.Neuron] -> [NN.Neuron] -> [[WeightFullInfo]] -> [AggConnection]
    makeConnections fromLayout toLayout weights = do
      let cooridinates = do
            to <- toLayout
            from <- fromLayout
            pure (from, to)

          connections = zip cooridinates (concat weights)

      (fromNeuron, toNeuron, weight) <- map (\((f, t), w) -> (f, t, w)) connections

      pure $
        AggConnection
          { fromNeuron = NN.idx fromNeuron,
            toNeuron = NN.idx toNeuron,
            minWeight = minW weight,
            maxWeight = maxW weight,
            sumWeight = sumW weight,
            avgWeight = sumW weight / fromIntegral (numW weight),
            countWeights = numW weight
          }
