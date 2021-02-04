{-
Copyright (C) 2020  Syed Moiz Ur Rehman

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

module CompilerUtilities.SourcePrinter
-- (
    -- printSource
-- )
where

import Data.List.NonEmpty

-- | Type class for source/writable string representation of AST nodes, IR and assembly.
-- This is separate from `Show` instances of those types since `show` is intended to be used for printing to console for debugging purposes.
class SourcePrinter a where
    printSource :: a -> String

printSourceList [] _ = ""
printSourceList [x] _ = printSource x
printSourceList (x:xs) c = printSource x ++ f xs
    where f = foldr (\a b -> c ++ printSource a ++ b) ""

printSourceNE (x:|[]) _ = printSource x
printSourceNE (x:|xs) c = printSource x ++ f xs
    where f = foldr (\a b -> c ++ printSource a ++ b) ""
