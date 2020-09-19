module Args where

import Options.Applicative
import Data.Semigroup ((<>))

data Args = Args {
    fileInput :: Input,
    genIR :: Bool,
    genSimplified :: Bool }

newtype Input = FileInput FilePath

input :: Parser Input
input = FileInput
    <$> strOption
        (   short 'f'
        <>  metavar "FILENAME"
        <>  help "Input file" )

emitIR :: Parser Bool
emitIR = switch
          ( short 'i'
         <> help "Whether to emit intermediate code" )

emitSimpleAST :: Parser Bool
emitSimpleAST = switch
          ( short 'c'
         <> help "Whether to emit simplified AST" )

parseArgs :: Parser Args
parseArgs = Args <$> input <*> emitIR <*> emitSimpleAST <**> helper