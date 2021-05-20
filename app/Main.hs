module Main where

import Data.Bifunctor (first)
import System.Environment ( getArgs )
import System.Exit ( exitFailure, exitSuccess )

import qualified Parser.AbsVinci as Parser (Program)
import Parser.LexVinci ( Token )
import Parser.ParVinci ( pProgram, myLexer )

import qualified Core.CoreManager as CM (CoreManager(..), map)
import Core.ConstDropping (dropConsts)
import Core.FrontendToCore (frontendProgramToCore)
import Core.LambdaLifting (lambdaLiftProgs)
import Core.TypeChecking (tcCoreManager)
import CPS.CoreToCPS (coreToCPS)
import qualified Frontend.AST as F
import Frontend.TranspileAST (transpile)
import SSA.CPStoSSA (cpsToSSA)
import SSA.OptimizeAndPrepare (optimizeAndPrepare)
import SPIRV.SpirManager (spirToLines)
import SPIRV.SSAtoSPIR (ssaToSpir)
import SpirDemo (compileToDemoSpir, compileToDemo2Spir)

type ParseFun a = [Token] -> Either String a
type FileName   = String

run :: ParseFun (Parser.Program a) -> String -> IO (Maybe F.Program)
run p s = case p ts of
    Left _ -> do 
        putStrLn "\nParse              Failed...\n"
        putStr "Tokens:"
        putStr $ show ts
        putStrLn s
        return Nothing
    Right tree -> return . pure $ transpile tree
    where
        ts = myLexer s

usage :: IO ()
usage = do
    putStrLn $ unlines
        [ "usage: Call with one of the following argument combinations:"
        , "  --help          Display this help message."
        , "  (files)         Compile content of files into SPIRV."
        , "  -f (files)      Compile content of files into Vinci frontend."
        , "  -c (files)      Compile content of files into Core."
        , "  -t (files)      Compile content of files into Core after typechecking."
        , "  -l (files)      Compile content of files into Core after Lambda Lifting."
        , "  -k (files)      Compile content of files into CPS."
        , "  -s (files)      Compile content of files into SSA."
        ]
    exitFailure

data OutputType = Frontend | Core | LiftedCore | TCCore | CPS | SSA | SPIRV | Demo2 | FullSPIRV

compilationFunction :: OutputType -> (F.Program -> String)
compilationFunction outputType = case outputType of
    Frontend   -> show
    Core       -> show . frontendProgramToCore
    rest       -> typeCheckAndCompile rest . dropConsts . frontendProgramToCore

typeCheckAndCompile :: OutputType -> CM.CoreManager Maybe -> String
typeCheckAndCompile outputType progs = case tcCoreManager progs of
    Left err -> err
    Right typeChecked -> case outputType of
        TCCore     -> show typeChecked
        LiftedCore -> show $ CM.map lambdaLiftProgs typeChecked
        CPS        -> show . coreToCPS $ CM.map lambdaLiftProgs typeChecked
        SSA        -> unlines . (show <$>) . optimizeAndPrepare $
              fst . cpsToSSA . coreToCPS $ CM.map lambdaLiftProgs typeChecked
        SPIRV      -> unlines . spirToLines . ssaToSpir . first optimizeAndPrepare $ 
            cpsToSSA . coreToCPS $ CM.map lambdaLiftProgs typeChecked
        Demo2      ->
            let spirManager = ssaToSpir . first optimizeAndPrepare $
                    cpsToSSA . coreToCPS $ CM.map lambdaLiftProgs typeChecked in
            compileToDemo2Spir spirManager
        FullSPIRV  ->
            let spirManager = ssaToSpir . first optimizeAndPrepare $ 
                     cpsToSSA . coreToCPS $ CM.map lambdaLiftProgs typeChecked in
            compileToDemoSpir spirManager
        _ -> "Error?"

parseFile :: OutputType -> FileName -> IO ()
parseFile outputType filename = do
    file <- readFile filename
    parseRes <- run pProgram file
    case parseRes of
        Nothing -> exitFailure
        Just toComp -> do
            putStrLn $ compilationFunction outputType toComp
            exitSuccess

main :: IO ()
main = do
    args <- getArgs
    case args of
        []         -> usage
        ["--help"] -> usage
        "-f":fs    -> mapM_ (parseFile Frontend) fs
        "-c":fs    -> mapM_ (parseFile Core) fs
        "-l":fs    -> mapM_ (parseFile LiftedCore) fs
        "-t":fs    -> mapM_ (parseFile TCCore) fs
        "-k":fs    -> mapM_ (parseFile CPS) fs
        "-s":fs    -> mapM_ (parseFile SSA) fs
        "-v":fs    -> mapM_ (parseFile SPIRV) fs
        "-2":fs    -> mapM_ (parseFile Demo2) fs
        fs         -> mapM_ (parseFile FullSPIRV) fs
