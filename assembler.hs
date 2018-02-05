
import Data.Word
import System.IO
import System.Environment
import Data.Map hiding (foldl, map, foldr)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.List (sortBy)
import Data.Bits
import Numeric (showHex, showIntAtBase)
import Data.Char (intToDigit)

type Address = String 

data Expr = Let Address Word8 
    | Label Address 
    | Load Address 
    | Sub Address 
    | Add Address 
    | Goto Address 
    | BN Address 
    | Store Address
    | Halt deriving Show

type CAddress = Word8

data CExpr = CLet Word8 
    | CLoad CAddress 
    | CSub CAddress 
    | CAdd CAddress 
    | CGoto CAddress 
    | CBN CAddress 
    | CStore CAddress
    | CHalt deriving Show

parse :: String -> Expr 
parse = _parse . words 
    where _parse ["let", a, b] = Let a (read b)
          _parse ["load", a] = Load a 
          _parse ["sub", a] = Sub a 
          _parse ["label", a] = Label a 
          _parse ["add", a] = Add a
          _parse ["goto", a] = Goto a 
          _parse ["halt"] = Halt 
          _parse ["bn", a] = BN a
          _parse ["store", a] = Store a

addresses :: [Word8]
addresses = [minBound, maxBound]

varReduce :: [Expr] -> ([Expr], Map Address Word8)
varReduce = foldr (\expr (exprs, bindings) -> case expr of 
    Let a v -> (exprs, insert a v bindings)
    e -> (e:exprs, bindings)) ([], empty)

labelReduce :: Word8 -> [Expr] -> (Word8, [Expr], Map Address Word8)
labelReduce offset = foldl (\(i, exprs, bindings) expr -> case expr of
    Label a -> (i, exprs, insert a i bindings)
    e -> (i+1, exprs ++ [e], bindings)) (offset, [], empty)

setAddresses :: [Expr] -> Map Address Word8 -> Map Address Word8 -> [CExpr]
setAddresses exprs varBindings labelBindings = map fix exprs 
    where fix (Sub var) = CSub $ findVar var
          fix (Add var) = CAdd $ findVar var 
          fix (Load var) = CLoad $ findVar var
          fix (Store var) = CStore $ findVar var 
          fix (Goto label) = CGoto $ findLabel label
          fix (BN label) = CBN $ findLabel label 
          fix Halt = CHalt 
          findVar var = fromMaybe (error $ "let " ++ var ++ " not found") (M.lookup var varBindings)
          findLabel label = fromMaybe (error $ "label " ++ label ++ " not found") (M.lookup label labelBindings)

_opcode :: CExpr -> Word8 
-- _opcode (CLet _) = 0
_opcode (CLoad _) = 0
_opcode (CSub _) = 1
_opcode (CAdd _) = 2
_opcode (CGoto _) = 3
_opcode (CBN _) = 4
_opcode (CStore _) = 6
_opcode CHalt = 7

opcode :: CExpr -> Word8 
opcode e = shift (_opcode e) 5

assembleCExpr :: CExpr -> Word8 
assembleCExpr (CLet x) = x
assembleCExpr CHalt = opcode CHalt 
assembleCExpr a@(CLoad x) = opcode a .|. x
assembleCExpr a@(CAdd x) = opcode a .|. x
assembleCExpr a@(CSub x) = opcode a .|. x
assembleCExpr a@(CGoto x) = opcode a .|. x
assembleCExpr a@(CBN x) = opcode a .|. x
assembleCExpr a@(CStore x) = opcode a .|. x

padZero :: String -> String 
padZero [c] = '0':[c]
padZero cs = cs

main :: IO () 
main = do
    [fname] <- getArgs 
    withFile fname ReadMode (\handle -> do
        contents <- hGetContents handle
        let lns = lines contents 
            (res, varValues) = varReduce $ map parse lns
            numVars = fromIntegral (length varValues)
            varList = sortBy (\(_, n) (_, m) -> n `compare` m) $ zip (keys varValues) [0..]
            -- varBindings = fromList $ zip (keys varValues) [1..]
            varBindings = fromList varList
            varCode = map (\(name, _) -> CLet . fromMaybe (error "woah") $ M.lookup name varValues) varList

            (_, res2, labelBindings) = labelReduce numVars res
            withAddresses = setAddresses res2 varBindings labelBindings
            assembledCode = map assembleCExpr (varCode ++ withAddresses)
            -- hex =map (padZero . (\x -> showIntAtBase 2 intToDigit x "")) assembledCode
            hex = map (padZero . (`showHex` "")) assembledCode
        putStrLn "v 2.1"
        putStrLn $ unwords hex )