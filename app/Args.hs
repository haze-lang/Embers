module Args where

import Options.Applicative
import Data.Char (toLower)
import Data.Semigroup ((<>))

data Args = Args
  { optInput :: Maybe Input
  , optQuiet :: Maybe License }

newtype Input = FileInput FilePath

data License = Full | Warranty | Conditions deriving Show

instance Read License where
    readsPrec _ r 
       | r == "license" = [(Full,[])]
       | r == "warranty" = [(Warranty,[])]
       | r == "conditions" = [(Conditions,[])]

fullLicense :: Parser License
fullLicense = flag' Full
    (  long "license"
    <> help "Show full license" )

warrantySection :: Parser License
warrantySection = flag' Warranty
    (  long "warranty"
    <> help "Show warranty section of license" )

conditionsSection :: Parser License
conditionsSection = flag' Conditions
    (  long "conditions"
    <> help "Show conditions sections of license" )

license :: Parser (Maybe License)
license = optional (fullLicense <|> warrantySection <|> conditionsSection)

input :: Parser (Maybe Input)
input = optional (FileInput <$> strOption
    (   short 'f'
    <>  metavar "FILENAME"
    <>  help "Input file" ))

parseArgs :: Parser Args
parseArgs = Args <$> input <*> license <**> helper