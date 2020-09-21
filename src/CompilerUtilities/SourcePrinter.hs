module CompilerUtilities.SourcePrinter
-- (
    -- printSource
-- )
where

import Data.List.NonEmpty
import Frontend.AbstractSyntaxTree

class SourcePrinter a where
    printSource :: a -> String

instance SourcePrinter Program where
    printSource (Program elements) = printSourceList elements "\n"

instance SourcePrinter TypeExpression where
    printSource (TVar v) = printSource v
    printSource (TArrow l r) = f l ++ " -> " ++ f r
        where f x = case x of
                TCons _ -> printSource x
                TVar _ -> printSource x
                _ -> "( " ++ printSource x ++ " )"
    printSource (TProd (x:|[])) = printSource x
    printSource (TProd (x:|xs)) =  "(" ++ printSource x ++ f xs ++ ")"
        where f = foldr (\a b -> " X " ++ printSource a ++ b) ""
    printSource (TCons s) = symStr s
    printSource (TApp s [ss]) = "TApp " ++ printSource s ++ " " ++ printSource ss

instance SourcePrinter Type where
    printSource t = show t

instance SourcePrinter ProgramElement where
    printSource (Ty t) = printSource t ++ "\n"
    printSource (Proc bParams retType name body) =
        let params = fmap fst bParams
            argTypes = fmap snd bParams
            procType = (TProd (fromList argTypes) `TArrow` retType)
        in
        symStr name ++ " : " ++ printSource procType ++ "\n"
        ++ symStr name ++ " " ++ printSourceList params " " ++ "\n"
        ++ "{\n" ++ printSourceNE body "\n" ++ "\n}\n"
    
    printSource (Func bParams retType name body) =
        let params = fmap fst bParams
            argTypes = fmap snd bParams
            funcType = (TProd (fromList argTypes) `TArrow` retType)
        in
        symStr name ++ " : " ++ printSource funcType ++ "\n"
        ++ symStr name ++ " " ++ printSourceList params " " ++ " = "
        ++ printSource body ++ "\n"

instance SourcePrinter Statement where
    printSource (Assignment l r) = symStr l ++ " = " ++ printSource r
    printSource (StmtExpr e) = printSource e

instance SourcePrinter Expression where
    printSource (Access e Tag) = "Tag(" ++ printSource e ++ ")"
    printSource (Access e (Member n)) = printSource e ++ "[" ++ show n ++ "]"
    printSource (Access e (ConsMember c n)) = printSource e ++ "." ++ show c ++"[" ++ show n ++ "]"
    printSource (App l r) = printSource l ++ " " ++ printSource r
    printSource (Cons cons []) = symStr cons
    printSource (Cons cons args) = symStr cons ++ " " ++ printSourceList args ", "
    printSource (Switch e cases def) = "switch " ++ printSource e ++ "\n" ++ concatMap _case (toList cases) ++ "\tdefault -> " ++ printSource def
        where
        _case (p, e) = "\t" ++ printSource p ++ " -> " ++ printSource e ++ "\n"
    printSource (Conditional c e1 e2) = "if " ++ printSource c ++ " then " ++ printSource e1 ++ " else " ++ printSource e2
    printSource (Lambda l) = lambda l
        where
        lambda (ProcLambda name params body) = printSourceNE params " " ++
            " => \n\t{\n\t\t" ++ printSourceNE body "\n\t\t" ++ "\n\t}\n"
        lambda (FuncLambda name params e) = printSourceNE params " " ++ " => " ++ printSource e ++ "\n"

    printSource (Tuple (x:|[])) = printSource x
    printSource (Tuple (x:|xs)) =  "(" ++ printSource x ++ f xs ++ ")"
        where f = foldr (\a b -> ", " ++ printSource a ++ b) ""
    printSource (Ident s) = symStr s
    printSource (Lit l) = printSource l

instance SourcePrinter Symbol where
    printSource (Symb id m) = printSource id ++ " at " ++ printSource m

instance SourcePrinter Parameter where
    printSource (Param s _) = symStr s

instance SourcePrinter Identifier where
    printSource (IDENTIFIER s) = s ++ "<>"
    printSource (ResolvedName id (s:|_)) = s ++ "<" ++ show id ++ ">"

instance SourcePrinter Metadata where
    printSource (Meta c l []) = show l ++ ":" ++ show c
    printSource (Meta c l f) = f ++ ":" ++ show l ++ ":" ++ show c

instance SourcePrinter Literal where
    printSource (NUMBER n) = show n
    printSource (CHAR c) = show c

printSourceList [] _ = ""
printSourceList [x] _ = printSource x
printSourceList (x:xs) c = printSource x ++ f xs
    where f = foldr (\a b -> c ++ printSource a ++ b) ""

printSourceNE (x:|[]) _ = printSource x
printSourceNE (x:|xs) c = printSource x ++ f xs
    where f = foldr (\a b -> c ++ printSource a ++ b) ""
