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

import System.IO
import Frontend.Parser (parseTokens)
import Frontend.Scanner (scan,isSpaceToken)
import Options.Applicative
import Args

prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine

file path = do
    content <- readFile path
    print (parseTokens $ filter (not.isSpaceToken) $ scan content)

repl = do
        input <- prompt "Embers>"
        print (scan input)
        repl

main :: IO ()
main = do
    putLicenseHeader
    args <- execParser (info parseArgs idm)
    run args

run :: Args -> IO ()
run (Args _ (Just l)) = case l of
    Full -> putLicense
    Warranty -> putWarranty
    Conditions -> putConditions

run (Args (Just (FileInput p)) _) = do
    file p

run _ = repl

putLicenseHeader = putStrLn ("Embers  Copyright (C) 2019  Syed Moiz Ur Rehman\n"++"This program comes with ABSOLUTELY NO WARRANTY; for details type `embers --warranty'.\n"++"This is free software, and you are welcome to redistribute it under\n"++"certain conditions; type `embers --conditions' for details.\n\n")

putLicense = putStrLn "Full License"

-- Warranty: Section 15
putWarranty = putStrLn ("  15. Disclaimer of Warranty.\n\n"++"  THERE IS NO WARRANTY FOR THE PROGRAM, TO THE EXTENT PERMITTED BY\nAPPLICABLE LAW.  EXCEPT WHEN OTHERWISE STATED IN WRITING THE COPYRIGHT\nHOLDERS AND/OR OTHER PARTIES PROVIDE THE PROGRAM \"AS IS\" WITHOUT WARRANTY\nOF ANY KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING, BUT NOT LIMITED TO,\nTHE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR\nPURPOSE.  THE ENTIRE RISK AS TO THE QUALITY AND PERFORMANCE OF THE PROGRAM\nIS WITH YOU.  SHOULD THE PROGRAM PROVE DEFECTIVE, YOU ASSUME THE COST OF\nALL NECESSARY SERVICING, REPAIR OR CORRECTION.")

-- Conditions: Section 2, 4, 5, 6
putConditions = putStrLn ("")