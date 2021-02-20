{-
Copyright (C) 2019  Syed Moiz Ur Rehman

This file is part of Embers.

Embers is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
any later version.

Embers is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Embers.  If not, see <https://www.gnu.org/licenses/>.
-}

module Main where

import Frontend.Error.CompilerError
import CompilerUtilities.AbstractSyntaxTree ( printMeta )
import CompilerUtilities.ProgramTable
import qualified CompilerUtilities.IntermediateProgram as IR
import CompilerUtilities.SourcePrinter

import Frontend.LexicalAnalysis.Scanner
import Frontend.SyntacticAnalysis.Parser
import Frontend.StaticAnalysis.NameResolver
import Frontend.TypeSystem.TypeChecker
import Frontend.Simplification.CpsTransformer
import Frontend.Simplification.AccessResolver
import Frontend.Simplification.LambdaResolver
import Frontend.Simplification.FunctionResolver
import Frontend.Simplification.RecursiveExpressionResolver
import MiddleEnd.IRGenerator
import Backend.AsmGeneration.AsmGenerator

import Options.Applicative
import System.Directory (makeAbsolute)
import System.FilePath.Windows (addExtension, takeFileName, replaceExtension)
import Args
import System.Console.ANSI
import Control.Monad (when)
import Backend.BackendArgs

analyze :: String -> String -> String -> Either [CompilerError] ProgramState
analyze sourceFilename std src = do
    stdTokens <- scanProcessed "" std
    srcTokens <- scanProcessed sourceFilename src
    ast <- parseTokensStdLib stdTokens srcTokens
    resolvedAst <- resolveNames ast
    typeCheck resolvedAst

main :: IO ()
main = do
    args <- execParser (info parseArgs idm)
    sourcePath <- makeAbsolute $ fileInput args
    stdLibPath <- makeAbsolute "..\\StandardLibrary.hz"
    let inFileName = takeFileName $ fileInput args
    outPath <- makeAbsolute $ fileOutput args

    source <- readFile sourcePath
    stdLib <- readFile stdLibPath

    case analyze inFileName stdLib source of
        Left errors -> printErrors errors
        Right pState -> do
            let cps = toCps pState
                lambdas = resolveLambdas cps
                functions = resolveFunctions lambdas
                constructions = resolveExpressions functions
                resolvedAccesses = resolvePatternMatches constructions
                ir = generateIR resolvedAccesses
                asm = generateAsm ir (asmArgs args)
            when (genParse args) $ writeFile (outPathwithExtension ".parse") $ printSource $ fst pState
            when (genSimplified args) $ writeFile (outPathwithExtension ".simple") $ printSource $ fst resolvedAccesses
            when (genIR args) $ writeFile (outPathwithExtension ".ir") $ printSourceList ir "\n\n"
            writeFile (outPathwithExtension ".asm") $ printSourceList asm "\n"

            where outPathwithExtension = addExtension outPath

printErrors :: [CompilerError] -> IO ()
printErrors errors = do
    putStrLnVividRed "Compilation Failed."
    mapM_ printError errors

    where
    printError (LexicalError e meta) = do
        putStr $ printMeta meta ++ " "
        putStrLnVividRed "Lexical Error:"
        print e

    printError (Error element meta error) = do
        putStr $ printMeta meta ++ " "
        let (header, msg) = case error of
                NameResolutionError e -> ("Error:", show e)
                ParseError e -> ("Syntax Error:", show e)
                TypeError e -> ("Type Error:", show e)
        putStrLnVividRed header
        putStrLn msg
    printError a = error $ show a

    putStrLnVividRed str = do
        setSGR [SetColor Foreground Vivid Red]
        putStrLn str
        setSGR [Reset]
