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

main :: IO ()
main = do
    putLicense


putLicense = putStrLn ("Embers  Copyright (C) 2019  Syed Moiz Ur Rehman\n"++"This program comes with ABSOLUTELY NO WARRANTY; for details type `show w'.\n"++"This is free software, and you are welcome to redistribute it\n"++"under certain conditions; type `show c' for details.")