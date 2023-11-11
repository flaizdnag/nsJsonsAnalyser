{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

{-|
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
module JsonAnalysis
    ( JsonToAnalyse (..)
    , analyseJsons
    ) where


import           Data.Aeson
import           Data.List
import qualified Data.Map              as Map
import           GHC.Generics
import           System.Directory
import           System.FilePath.Posix
import           System.IO


import           JsonHandling          as JH
import           LPjsons
import           LogicPrograms         as LP
import           NNdecoder
import           NNjsons
import           NNjsons_python        as NNpy
import           NNjsons_python1       as NNpy1
import           NeuralNetworks        as NN


data JsonToAnalyse = JsonToAnalyse
    { lp_before              :: LPjson
    , lp_before_params       :: LPparams
    , neural_network_factors :: Factors
    , nn_recipe              :: NNwithAmin
    , nn_before              :: NNpython
    , nn_after               :: NNpython
    , errors                 :: [Float]
    , io_pairs               :: [[[Float]]]
    , lp_after               :: LPafter
    , lp_after_params        :: LPparams
    } deriving (Show, Read, Generic)

instance FromJSON JsonToAnalyse
instance ToJSON JsonToAnalyse where
    toEncoding = genericToEncoding defaultOptions


analyseJsons :: IO ()
analyseJsons = do
    let checkedLPsFile = "logic_programs.txt"
    writeFile checkedLPsFile ""
    putStrLn $ "Created file " ++ checkedLPsFile ++ " with logic programs"
    putStrLn $ "Writing logic programs to " ++ checkedLPsFile

    files <- listDirectory "results"

    lps <- unlines . map (\(x, y) -> "Number of lps: " ++ show y ++ "\n" ++ show x ++ "\n") <$> recAnalyser files (return [])
    appendFile checkedLPsFile lps

    putStrLn "Done"


recAnalyser :: [FilePath] -> IO [(LPjson, Int)] -> IO [(LPjson, Int)]
recAnalyser [] accIO = accIO
recAnalyser (file:files) accIO = recAnalyser files newAcc
    where
        newAcc = do
            acc <- accIO
            lp  <- lpIO

            let accMap = Map.fromList acc

            case Map.lookup lp accMap of
                Nothing  -> return $ Map.toList $ Map.insert lp 1 accMap
                Just val -> return $ Map.toList $ Map.insert lp (val + 1) accMap

        lpIO = do
            json <- decodeFileStrict ("results/" ++ file) :: IO (Maybe JsonToAnalyse)
            case json of
                Nothing -> undefined
                --Nothing -> error "Could not process file " ++ file
                Just xs -> do
                    let lp_to_save = LPjsons.lp $ lp_after xs
                        --nn_to_save = nn_after xs
                        --data_to_save =
                            --show lp_to_save ++ "\n" ++ show nn_to_save
                    return lp_to_save


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

{-
exampleJson = JsonToAnalyse
    { lp_before        = exampleLP
    --, lp_before_params = exampleLPparams
    , nn_recipe        = exampleNNrecipe
    --, nn_before_params = exampleNNparams
    , nn_before        = exampleNNpy
    , nn_after         = exampleNNpy
    , errors           = []
    , io_pairs         = []
    , lp_after         = exampleLPafter
    --, lp_after_params  = exampleLPparams
    }

exampleLP = LPtoNN
    { JH.lp = makeLPjson
        [ Fact { clHead = A { LP.idx = 1, LP.label = ""} }
        , Cl
            { clHead   = A { LP.idx = 2, LP.label = "" }
            , clPAtoms = [ A { LP.idx = 1, LP.label = "" } ]
            , clNAtoms = [ A { LP.idx = 3, LP.label = "" } ]
            }
        ]
    , abductive_goal = exampleAbdGoal
    --, JH.factors = exampleFactors
    }

exampleAbdGoal = Cl
    { clHead = A { LP.idx = 1, LP.label = "" }
    , clPAtoms = []
    , clNAtoms = []
    }

exampleLPparams = LPparams
    { LPjsons.clauses = ClausesData
        { amount        = 5
        , onlyPos       = 5
        , onlyNeg       = 0
        , mix           = 0
        , headWithH     = 0
        , LPjsons.atoms = AtomsData
            { LPjsons.sum = 0
            , pos         = 5
            , neg         = 0
            , withH       = 0
            , posWithH    = 0
            , negWithH    = 0
            }
        , difAtoms = AtomsData
            { LPjsons.sum = 3
            , pos         = 3
            , neg         = 0
            , withH       = 0
            , posWithH    = 0
            , negWithH    = 0
            }
        , atomsInHeadsNotBodies =
            [ A {LP.idx = 1, LP.label = ""}
            , A {LP.idx = 1, LP.label = ""}
            , A {LP.idx = 4, LP.label = ""}
            ]
        , atomsInBodiesNotInHeads =
            [ A {LP.idx = 5, LP.label = ""}
            , A {LP.idx = 5, LP.label = ""}
            ]
        , numOfPosAtomsInEachClause             = [1, 1, 1, 1, 1]
        , numOfNegAtomsInEachClause             = [0, 0, 0, 0, 0]
        , numOfClausesWhoseHeadAppearsInTheBody = 0
        , numOfClausesWhoseHeadAppearsInABody   = 2
        }
    , LPjsons.facts       = 0
    , LPjsons.assumptions = 0
    }

exampleFactors = Factors
    { JH.beta = 1.0
    , ahln    = 1
    , r       = 0.005
    , JH.bias = 0.0
    , w       = 0.1
    , JH.amin = 0.1
    }

exampleNNrecipe = NNwithFactors
    { nn        = exampleNN
    , nnFactors = exampleFactors
    }

exampleNN = NN
    { NN.inpLayer            = []
    , NN.hidLayer            = []
    , NN.outLayer            = []
    , NN.recLayer            = []
    , NN.inpToHidConnections = []
    , NN.hidToOutConnections = []
    , NN.recConnections      = []
    }

exampleNNparams = NNparams
    { NNjsons.atoms = NNatoms
        { NNpy.sum = 22
        , NNpy.inp = 6
        , NNpy.hid = 11
        , NNpy.out = 5
        , NNpy.rec = 0
        }
    , connections = NNconnections
        { NNjsons.inp2Hid = 26
        , NNjsons.hid2Out = 14
        , NNjsons.rec     = 5
        }
    , bigWeights = NNbigWeights
        { NNpy1.inp2Hid = 6
        , NNpy1.hid2Out = 5
        , NNpy1.rec     = 5
        }
    , NNjsons.factors = exampleFactors
    }

exampleNNpy = NNpython
    { architecture    = NNarchitecture
        { NNpy.inpLayer            = []
        , NNpy.hidLayer            = []
        , NNpy.outLayer            = []
        , NNpy.recLayer            = []
        , NNpy.inpToHidConnections = []
        , NNpy.hidToOutConnections = []
        , NNpy.recConnections      = []
        }
    , i2h_connections = []
    , h2o_connections = []
    , o2i_connections = []
    , NNpy.factors    = exampleFactors
    , comments        = []
    }

exampleLPafter = LPafter
    { LPjsons.lp = makeLPjson
        [ Fact { clHead = A { LP.idx = 1, LP.label = ""} }
        , Cl
            { clHead   = A { LP.idx = 2, LP.label = "" }
            , clPAtoms = [ A { LP.idx = 1, LP.label = "" } ]
            , clNAtoms = [ A { LP.idx = 3, LP.label = "" } ]
            }
        ]
    }

examplePrinting :: IO ()
examplePrinting = do
    encodeFile "test.json" exampleJson


makeLPjson :: LP -> LPjson
makeLPjson xs = LPjson
    { JH.facts       = filter isFact xs
    , JH.assumptions = filter isAssumption xs
    , JH.clauses     = filter isClause xs
    }
    where
        isFact x = case x of
            Fact _ -> True
            _      -> False
        isAssumption x = case x of
            Assumption _ -> True
            _            -> False
        isClause x = case x of
            Cl {} -> True
            _     -> False


jsonAnalyser :: FilePath -> FilePath -> IO ()
jsonAnalyser pathToAnalyse pathToCheck = do
    doneLPs <- readFile pathToCheck
    content <- (decodeFileStrict pathToAnalyse ::  IO (Maybe JsonToAnalyse))
    case content of
        Nothing -> putStrLn "Cannot decode json"
        Just js  -> do
            putStrLn "Json decoded"
            if show (lp_after js) `isInfixOf` doneLPs then do
                putStrLn "LP already there"
                appendFile pathToCheck ""
            else
                appendFile pathToCheck (show $ lp_after js)

-}
