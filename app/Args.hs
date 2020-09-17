module Args where

import Options.Applicative
import Data.Semigroup ((<>))

data Args = Args { optInput :: Input }

newtype Input = FileInput FilePath

input :: Parser Input
input = (FileInput <$> strOption
    (   short 'f'
    <>  metavar "FILENAME"
    <>  help "Input file" ))

parseArgs :: Parser Args
parseArgs = Args <$> (helper <*> input)