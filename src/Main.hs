module Main where

import Data.List.Split
import Data.List
import Numeric
import Data.Monoid
import Data.Char (intToDigit, isSpace)
-- import Text.Read

destToBinary :: String -> Maybe String
destToBinary "" = Just "000"
destToBinary "0" = Just "000"
destToBinary "M" = Just "001"
destToBinary "D" = Just "010"
destToBinary "MD" = Just "011"
destToBinary "A" = Just "100"
destToBinary "AM" = Just "101"
destToBinary "AD" = Just "110"
destToBinary "AMD" = Just "111"
destToBinary _ = Nothing

compToBinary :: String -> Maybe String
compToBinary "0" = Just "0101010"
compToBinary "1" = Just "0111111"
compToBinary "-1" = Just "0111010"
compToBinary "D" = Just "0001100"
compToBinary "A" = Just "0110000"
compToBinary "!D" = Just "0001101"
compToBinary "!A" = Just "0110001"
compToBinary "-D" = Just "0001111"
compToBinary "-A" = Just "0110011"
compToBinary "D+1" = Just "0011111"
compToBinary "A+1" = Just "0110111"
compToBinary "D-1" = Just "0001110"
compToBinary "A-1" = Just "0110010"
compToBinary "D+A" = Just "0000010"
compToBinary "D-A" = Just "0010011"
compToBinary "A-D" = Just "0000111"
compToBinary "D&A" = Just "0000000"
compToBinary "D|A" = Just "0010101"
compToBinary "M" = Just "1110000"
compToBinary "!M" = Just "1110001"
compToBinary "-M" = Just "1110011"
compToBinary "M+1" = Just "1110111"
compToBinary "M-1" = Just "1110010"
compToBinary "D+M" = Just "1000010"
compToBinary "D-M" = Just "1010011"
compToBinary "M-D" = Just "1000111"
compToBinary "D&M" = Just "1000000"
compToBinary "D|M" = Just "1010101"
compToBinary _ = Nothing

jumpToBinary :: String -> Maybe String
jumpToBinary "" = Just "000"
jumpToBinary "JGT" = Just "001"
jumpToBinary "JEQ" = Just "010"
jumpToBinary "JGE" = Just "011"
jumpToBinary "JLT" = Just "100"
jumpToBinary "JNE" = Just "101"
jumpToBinary "JLE" = Just "110"
jumpToBinary "JMP" = Just "111"
jumpToBinary _ = Nothing

symbolTable :: [(String, String)]
symbolTable =
  [("SP", "0")
  ,("LCL", "1")
  ,("ARG", "2")
  ,("THIS", "3")
  ,("THAT", "4")
  ,("R0", "0")
  ,("R1", "1")
  ,("R2", "2")
  ,("R3", "3")
  ,("R4", "4")
  ,("R5", "5")
  ,("R6", "6")
  ,("R7", "7")
  ,("R8", "8")
  ,("R9", "9")
  ,("R10", "10")
  ,("R11", "11")
  ,("R12", "12")
  ,("R13", "13")
  ,("R14", "14")
  ,("R15", "15")
  ,("SCREEN", "16384")
  ,("KBD", "24576" )]

cleanComments :: [String] -> [String]
cleanComments =  fmap (\line -> first . splitOn "//" $ line)
  where first (x:_) = x
        first _ = []

cleanSpaces :: [String] -> [String]
cleanSpaces = filter (\line -> line /= "\r" && line /= "")

cleanEndOfLine :: [String] -> [String]
cleanEndOfLine = fmap (\line -> filter (/='\r') line)

cleanFile :: [String] -> [String]
cleanFile = map strip . cleanEndOfLine . cleanSpaces . cleanComments
 where f = reverse . dropWhile isSpace
       strip = f . f

fillWith0s :: String -> String
fillWith0s string
  | length string == 15 = string
  | otherwise = fillWith0s ("0" ++ string)

convertTo15BitBinary :: String -> String
convertTo15BitBinary number = fillWith0s $ showIntAtBase 2 intToDigit (read number :: Integer) ""

firstOrEmpty :: [String] -> String
firstOrEmpty (x:_) = x
firstOrEmpty _ = ""

secondOrEmpty :: [String] -> String
secondOrEmpty (_:x:_) = x
secondOrEmpty _ = ""

thirdOrEmpty :: [String] -> String
thirdOrEmpty (_:_:x:[]) = x
thirdOrEmpty _ = ""

type Dest = String
type Comp = String
type Jump = String
type LineNumber = Integer

data ComposedInstruction = DestCompJump [String] | CompJump [String] | DestComp [String] | CompOnly String

data Instruction = AInst LineNumber String | CInst LineNumber Dest Comp Jump  | Symbol LineNumber String | Loop LineNumber String deriving Show

splitC :: String -> ComposedInstruction
splitC inst
  | (isInfixOf "=" inst) && (isInfixOf ";" inst) = DestCompJump (concatMap (splitOn "=") .  splitOn ";" $ inst)
  | isInfixOf ";" inst = CompJump (splitOn ";" inst)
  | isInfixOf "=" inst = DestComp (splitOn "=" inst)
  | otherwise = CompOnly inst

buildC :: ComposedInstruction -> Instruction
buildC (DestCompJump insts) = CInst 0 (firstOrEmpty insts) (secondOrEmpty insts) (thirdOrEmpty insts)
buildC (CompJump insts) = CInst 0 "" (firstOrEmpty insts) (secondOrEmpty insts)
buildC (DestComp insts) = CInst 0 (firstOrEmpty insts) (secondOrEmpty insts) ""
buildC (CompOnly insts) = CInst 0 "" insts ""

buildInstructions :: String -> Instruction
buildInstructions ('@':x:rest) = if x `elem` "0123456789" then AInst 0 (x:rest) else Symbol 0 (x:rest)
buildInstructions ('(':rest) = Loop 0 (filter (/=')') rest)
buildInstructions cInstruction = buildC . splitC $ cInstruction

addLineNumbers :: [Instruction] -> [Instruction]
addLineNumbers instructions = go instructions 0
  where
    addNumber (AInst _ b) n = AInst n b
    addNumber (Loop _ a) n = Loop n a
    addNumber (Symbol _ a) n = Symbol n a
    addNumber (CInst _ a b c) n = CInst n a b c
    go [] _ = []
    go (x@(Loop _ _):xs) number = addNumber x number : go xs number
    go (x:xs) number = addNumber x number : go xs (number+1)

toBinary :: Instruction -> Maybe String
toBinary (AInst _ inst) = Just ("0" ++  (convertTo15BitBinary inst))
toBinary (CInst _ dest comp jump) =  Just "111" <> compToBinary comp <> destToBinary dest <> jumpToBinary jump
toBinary _ = Nothing

instructionsToBinary :: [Instruction] -> [String]
instructionsToBinary = filter (not . null) . map (toBinaryString . toBinary)
  where toBinaryString Nothing = ""
        toBinaryString (Just string) = string

symbolsToAInst :: [Instruction] -> [Instruction]
symbolsToAInst = map toAInt
  where
    parsed _ ln (Just (_, value)) = AInst ln value
    parsed symb ln Nothing =  Symbol ln symb
    toAInt (Symbol ln symb) = parsed symb ln . find (\(symbFromTable, _) -> symbFromTable == symb) $ symbolTable
    toAInt x = x

findLoop :: String -> [Instruction] -> Maybe Instruction
findLoop symb = find matchingLoop
  where matchingLoop (Loop _ name) = symb == name
        matchingLoop _ = False

loopSymbsToAInst :: [Instruction] -> [Instruction]
loopSymbsToAInst instructions = map toAInt instructions
  where
    parsed _ ln (Just (Loop loopLn _))  = AInst ln (show loopLn)
    parsed _ _ (Just x) = x
    parsed symb ln Nothing =  Symbol ln symb
    toAInt (Symbol ln symb) = parsed symb ln . findLoop symb $ instructions
    toAInt x = x

mnemToAinst :: [Instruction] -> [Instruction]
mnemToAinst = go 16
  where
    go _ [] = []
    go n (Symbol ln name:xs) = AInst ln (show n) : go (n+1) (map (sameInstances name n) xs)
    go n (x:xs) =  x : go n xs
    sameInstances name n symb@(Symbol ln x) = if name == x then AInst ln (show n) else symb
    sameInstances _ _ x = x


convertItAll :: String -> [Instruction]
convertItAll  =
  mnemToAinst . loopSymbsToAInst . symbolsToAInst . addLineNumbers . map buildInstructions . cleanFile . lines

main :: IO ()
main = do
  content <- readFile "../06/rect/Rect.asm"
  -- print . unlines . instructionsToBinary . addLineNumbers . map buildInstructions . cleanFile . lines $ content
  -- print . map buildInstructions . cleanFile . lines $content
  -- print . convertItAll $  content
  -- print . instructionsToBinary . convertItAll $  content
  writeFile "../06/Rect.hack" . unlines . instructionsToBinary . convertItAll $ content
