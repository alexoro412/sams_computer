{-# LANGUAGE BangPatterns #-}
import Data.Word
import System.IO
import System.Environment
import Data.Map () -- hiding (foldl, map, foldr, filter)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, listToMaybe)
import Data.List (sortBy)
import Data.Bits
import Numeric (showHex)
import System.Exit (exitFailure)
import Control.Monad (when)
import Data.Char (chr)

type Address = String 

data Expr = Let Address Int 
    | Label Address 
    | Load Address 
    | Sub Address 
    | Add Address 
    | Goto Address 
    | BN Address 
    | Store Address
    | Halt deriving Show

type CAddress = Word8

data CExpr = CExpr Op Word8 deriving Show 

data Op = CHalt | CLoad | CSub | CAdd | CGoto | CBN | CStore | CLet deriving Show

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
addresses = [minBound .. maxBound]

varReduce :: [Expr] -> ([Expr], M.Map Address Int)
varReduce = foldr (\expr (exprs, bindings) -> case expr of 
    Let a v -> (exprs, M.insert a v bindings)
    e -> (e:exprs, bindings)) ([], M.empty)

labelReduce :: Word8 -> [Expr] -> (Word8, [Expr], M.Map Address Word8)
labelReduce offset = foldl (\(i, exprs, bindings) expr -> case expr of
    Label a -> (i, exprs, M.insert a i bindings)
    e -> (i+1, exprs ++ [e], bindings)) (offset, [], M.empty)

setAddresses :: [Expr] -> M.Map Address Word8 -> M.Map Address Word8 -> [CExpr]
setAddresses exprs varBindings labelBindings = map fix exprs 
    where fix (Sub var) = CExpr CSub $ findVar var
          fix (Add var) = CExpr CAdd $ findVar var 
          fix (Load var) = CExpr CLoad $ findVar var
          fix (Store var) = CExpr CStore $ findVar var 
          fix (Goto label) = CExpr CGoto $ findLabel label
          fix (BN label) = CExpr CBN $ findLabel label 
          fix Halt = CExpr CHalt 0
          findVar var = fromMaybe (error $ "let " ++ var ++ " not found") (M.lookup var varBindings)
          findLabel label = fromMaybe (error $ "label " ++ label ++ " not found") (M.lookup label labelBindings)

_opcode :: Op -> Word8 
_opcode CLet = 0
_opcode CLoad = 9
_opcode CSub = 1
_opcode CAdd = 2
_opcode CGoto = 3
_opcode CBN = 4
_opcode CStore = 6
_opcode CHalt = 7

opcode :: Op -> Word8 
opcode e = shift (_opcode e) 5

-- Makes sure no addresses go over 5 bits
clamp :: Word8 -> Word8 
clamp = min 31 

assembleCExpr :: CExpr -> Word8 
assembleCExpr (CExpr CLet x) = x
assembleCExpr (CExpr e x) = opcode e .|. clamp x

padZero :: String -> String 
padZero [c] = '0':[c]
padZero cs = cs

esc :: Char 
esc = chr 27

red :: String -> String 
red s = esc:"[31m" ++ s ++ esc:"[0m"

green :: String -> String
green s = esc:"[32m" ++ s ++ esc:"[0m"

bold :: String -> String 
bold s = esc:"[1m" ++ s ++ esc:"[0m"

main :: IO () 
main = do
    -- Argument Parsing
    args <- getArgs 
    (fname, output) <- case listToMaybe args of 
        -- If filename provided, check for output 
        -- If no output, use "a.out"
        Just fname -> let (_:args2) = args in return (fname, fromMaybe "a.out" $ listToMaybe args2) 
        -- No filename, so print usage and exit
        Nothing -> do
            putStrLn "Error: No filename provided"
            putStrLn "Usage: assembler <fname> [<output>]"
            exitFailure 
    

    hex <- withFile fname ReadMode (\handle -> do
        contents <- hGetContents handle
        let lns = map unwords . filter (/= []) . map words $ lines contents 
            -- Remove variable declarations
            (noVars, varValues_Int) = varReduce $ map parse lns
            -- Convert [Int] to [Word8]
            varValues = M.map fromIntegral varValues_Int

        -- Warn of any variables that are too large
        mapM_ (\(name, value) -> when (value > 31) $ putStrLn $ red "[Warning] " ++ "Var " ++ bold name ++ " was set to " ++ bold (show value) ++ ". It will be replaced with " ++ bold (show . clamp $ fromIntegral value)) $ M.toList varValues_Int
       
        let numVars = fromIntegral (length varValues)  -- Count number of variables
        
            -- Assign an address to each variable
            varAddresses = sortBy (\(_, n) (_, m) -> n `compare` m) $ zip (M.keys varValues) addresses
            -- Generate a map of variable name -> CAddress 
            varBindings = M.fromList varAddresses
            -- Generate CExpr list with vars at their addresses
            varCode = map (\(name, _) -> CExpr CLet . fromMaybe (error "woah") $ M.lookup name varValues) varAddresses

            -- Remove labels, generate map from label name -> CAddress 
            (_, noLabels, labelBindings) = labelReduce numVars noVars
            -- insert proper CAddress's for each CExpr 
            withAddresses = setAddresses noLabels varBindings labelBindings
            -- Assemble code into hex
            -- Strictness to allow file to be closed before next file written to
            !hex = map (padZero . (`showHex` "") . assembleCExpr) (varCode ++ withAddresses)

        -- Warn when program is too long
        when (length withAddresses > 256) $ do
             putStrLn $ red "[Warning]" ++ " Program takes up too much space"
             putStrLn $     "          Maximum space is " ++ bold "256" ++ " bytes"
             putStrLn $     "          Program uses " ++ bold (show $ length withAddresses) ++ " bytes"
        return $ unwords hex)

    -- Write output
    writeFile output $ "v 2.1\n" ++ hex

    -- Say where it was written!
    putStrLn . green $ "Output written to ./" ++ output
                
    