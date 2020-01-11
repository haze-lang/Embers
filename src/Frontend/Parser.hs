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

module Frontend.Parser where

import Frontend.SyntaxTree
import Frontend.AbstractParser
import Control.Applicative

type Parser a = Frontend.AbstractParser.AbsParser [Token] Metadata a

-- Consume one Token
item :: Parser (Token, Metadata)
item = P (\inp -> case inp of
            ([], m) -> Right m
            ((x:xs), m) -> Left ((x, m), (xs, m))
        )


