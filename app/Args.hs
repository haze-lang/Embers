module Args where

import Backend.BackendArgs

import Options.Applicative

data Args = Args {
    fileInput :: String,
    fileOutput :: String,
    genIR :: Bool,
    genSimplified :: Bool,
    genParse :: Bool,
    asmArgs :: AsmGeneratorArgs }

asmGen :: Parser AsmGeneratorArgs
asmGen = AsmGeneratorArgs
        <$> switch
        (   long "irc"
        <>  help "Add IR comments in generated assembly" )
        <*> switch
        (   long "src"
        <>  help "Add source comments in generated assembly" )

output :: Parser String
output = strOption
        (   short 'o'
        <>  value "..\\Output"
        <>  metavar "OUT"
        <>  help "Output file" )

input :: Parser String
input = strOption
        (   short 'f'
        <>  value "..\\Haze.hz"
        <>  metavar "FILENAME"
        <>  help "Input file" )

emitIR :: Parser Bool
emitIR = switch
          ( long "ir"
         <> help "Emit internal IR" )

emitParse :: Parser Bool
emitParse = switch
          ( long "parse"
         <> help "Emit parsed AST" )

emitSimpleAST :: Parser Bool
emitSimpleAST = switch
          ( long "ast"
         <> help "Emit simplified AST" )

parseArgs :: Parser Args
parseArgs = Args <$> input <*> output <*> emitIR <*> emitSimpleAST <*> emitParse <*> asmGen <**> helper
