module Main where

import System.Environment ( getArgs )
import System.Exit ( exitFailure, exitSuccess )

import qualified Parser.AbsVinci as Parser (Program)
import Parser.LexVinci ( Token )
import Parser.ParVinci ( pProgram, myLexer )

import Core.FrontendToCore (frontendProgramToCore)
import CPS.CoreToCPS (coreToCPS)
import qualified Frontend.AST as F
import Frontend.TranspileAST (transpile)
import SSA.CPStoSSA (cpsToSSA)
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
        , "  -k (files)      Compile content of files into CPS."
        , "  -s (files)      Compile content of files into SSA."
        ]
    exitFailure

data OutputType = Frontend | Core | CPS | SSA | SPIRV | FullSPIRV

compilationFunction :: OutputType -> (F.Program -> String)
compilationFunction outputType = case outputType of
    Frontend  -> show
    Core      -> show . frontendProgramToCore
    CPS       -> \x -> unlines $ show . coreToCPS <$> frontendProgramToCore x
    SSA       -> \x -> unlines $ show . cpsToSSA . coreToCPS <$> frontendProgramToCore x
    SPIRV     -> \x -> unlines $ unlines . fmap show . ssaToSpir . cpsToSSA . coreToCPS <$> frontendProgramToCore x
    FullSPIRV -> \x -> 
        let spir = head $ ssaToSpir . cpsToSSA . coreToCPS <$> frontendProgramToCore x in
        compileToDemoSpir spir

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
        "-k":fs    -> mapM_ (parseFile CPS) fs
        "-s":fs    -> mapM_ (parseFile SSA) fs
        "-v":fs    -> mapM_ (parseFile SPIRV) fs
        fs         -> mapM_ (parseFile FullSPIRV) fs
