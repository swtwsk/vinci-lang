module Main where

import System.Environment ( getArgs )
import System.Exit ( exitFailure, exitSuccess )

import qualified Parser.AbsVinci as Parser (Program)
import Parser.LexVinci ( Token )
import Parser.ParVinci ( pProgram, myLexer )

import qualified Core.AST as Core (Prog)
import Core.FrontendToCore (frontendProgramToCore)
import Core.LambdaLifting (lambdaLiftProgs)
import Core.TypeChecking (tcProgs)
import CPS.CoreToCPS (coreToCPS)
import qualified Frontend.AST as F
import Frontend.TranspileAST (transpile)
import SSA.CPStoSSA (cpsToSSA)
import SSA.Optimizations.GraphOptimizations (optimizeGraph)
import SPIRV.SSAtoSPIR (ssaToSpir)
import SpirDemo (compileToDemoSpir)

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

data OutputType = Frontend | Core | LiftedCore | TCCore | CPS | SSA | SPIRV | FullSPIRV

compilationFunction :: OutputType -> (F.Program -> String)
compilationFunction outputType = case outputType of
    Frontend   -> show
    Core       -> show <$> frontendProgramToCore
    rest       -> typeCheckAndCompile rest . frontendProgramToCore
    -- LiftedCore -> \x -> unlines $ show <$> (lambdaLiftProg =<< frontendProgramToCore x)
    -- rest       -> \x -> typeCheckAndCompile rest (lambdaLiftProg =<< frontendProgramToCore x)

typeCheckAndCompile :: OutputType -> [Core.Prog Maybe] -> String
typeCheckAndCompile outputType progs = case tcProgs progs of
    Left err -> err
    Right typeChecked -> case outputType of
        TCCore     -> unlines $ show <$> typeChecked
        LiftedCore -> unlines $ show <$> lambdaLiftProgs typeChecked
        CPS        -> unlines $ show . coreToCPS <$> lambdaLiftProgs typeChecked
        SSA        -> unlines $ 
            show . optimizeGraph . cpsToSSA . coreToCPS <$> 
                lambdaLiftProgs typeChecked
        SPIRV      -> unlines . fmap show . uncurry (++) . ssaToSpir $ 
            optimizeGraph . cpsToSSA . coreToCPS <$> typeChecked
        FullSPIRV  ->
            let (constsTypes, fnOps) = ssaToSpir $ 
                    optimizeGraph . cpsToSSA . coreToCPS <$> 
                        lambdaLiftProgs typeChecked in
            compileToDemoSpir constsTypes fnOps
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
        fs         -> mapM_ (parseFile FullSPIRV) fs
