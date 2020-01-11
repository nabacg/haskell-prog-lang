module BFEvaluator (main, loadFile, topLevelEval, topLevelParse, BfCmd(..)) where

import System.IO

import Text.ParserCombinators.Parsec
import Data.Char
import Data.List(intercalate)
import ListZipper


data BfCmd = IncPtrCmd
        | DecPtrCmd
        | IncCurrByteCmd
        | DecCurrByteCmd
        | PrintByte
        | ReadByte
        | LoopCmd [BfCmd] deriving (Show, Eq)

singleBfCmdParser :: Parser BfCmd
singleBfCmdParser =  const IncPtrCmd <$> char '>'
           <|> const DecPtrCmd <$> char '<'
           <|> const IncCurrByteCmd <$> char '+'
           <|> const DecCurrByteCmd <$> char '-'
           <|> const PrintByte <$> char '.'
           <|> const ReadByte <$> char ','
           <|> loopCmdParser

loopCmdParser :: Parser BfCmd
loopCmdParser = do
               char '['
               cmds <- many singleBfCmdParser
               char ']'
               return $ LoopCmd cmds


bfCmdParser :: Parser BfCmd
bfCmdParser =  singleBfCmdParser

topLevelParser = many1 bfCmdParser

topLevelParse :: String -> Either ParseError [BfCmd]
topLevelParse programString = parse topLevelParser "" $ stripNonCmds programString

stripNonCmds = filter (`elem` "+-<>[],.")

type Env = ListZipper Int
type EState = (Env, Int, [Char], [(Int, Char)])


eval :: EState -> BfCmd -> EState
eval (env, c, stdIn, stdOut) _    | c < 0     = (env, c, stdIn, stdOut)
eval (env, c, stdIn, stdOut) (IncPtrCmd)      = (moveRight env, c-1, stdIn, stdOut)
eval (env, c, stdIn, stdOut) (DecPtrCmd)      = (moveLeft env,  c-1, stdIn, stdOut)
eval (env, c, stdIn, stdOut) (IncCurrByteCmd) = (incCurr env,   c-1, stdIn, stdOut)
eval (env, c, stdIn, stdOut) (DecCurrByteCmd) = (decCurr env, c-1, stdIn, stdOut)
eval (env, c, stdIn, stdOut) (PrintByte)      = (env, c-1, stdIn,  (c-1, chr $ getCurr env) : stdOut)
eval (env, c, (i:stdIn), stdOut) (ReadByte)   = (setCurr (ord i) env, c-1, stdIn, stdOut)
eval (env, c, [], _)         (ReadByte)       = (env, c, [], [(c, 'E')]) -- ToDo better way to print this error!
eval (env, c, stdIn, stdOut) (LoopCmd cmds) = if getCurr env == 0 -- [ open loop
                                                then (env, c-2, stdIn, stdOut) -- ] close loop, dec counter by for each '[' and  ']' operation
                                                else
                                                  if getCurr env' == 0 || c'-1 < 0 -- ]  close loop test, counter is dec'ed in s'
                                                  then s'
                                                  else eval s' (LoopCmd cmds)
                                                  where
                                                    (env', c', stdIn', stdOut') = foldl (\acc cmd -> eval acc cmd) (env, c-1, stdIn, stdOut) cmds -- inside of loop, dec for each cmd in cmds
                                                    s' = (env', c'-1, stdIn', stdOut') -- close loop state, dec counter for ']'

topLevelEval :: String -> String -> String
topLevelEval programString stdIn = case topLevelParse programString of
   Left e -> error $ show e
   Right cmds -> case foldl (\s c -> eval s c) ((zipperOf 0), 100000, stdIn, []) cmds of
                   (_, c, _, stdOut) | c < 0  -> (map snd $ takeWhile (\(cnt, _) -> cnt >= 0) $  reverse stdOut) ++ "\nPROCESS TIME OUT. KILLED!!!" -- ++ " c=" ++ show c ++ " stdOut=\n"  ++ (intercalate "\n" $ map show $ reverse stdOut)
                   (_, c, _, stdOut) -> map snd $ reverse stdOut

processInputs :: [String] -> IO String
processInputs inputLines =
  if null inputLines then return "ERROR: Empty input"
  else
    do
      let (m:n:[]) =  map read $ (words . head) inputLines :: [Int]
      let stdIn = take m  $ (head . drop 1) inputLines
      return $ topLevelEval (intercalate "" ((take n . drop 2) inputLines)) stdIn

loadFile :: String -> IO String
loadFile path = lines <$> readFile path >>= processInputs

main :: IO ()
main = lines <$> getContents >>= processInputs >>= putStrLn
