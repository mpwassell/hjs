-- |
-- HJS Haskell Javascript Interpreter
-- (c) Mark Wassell 2007
-- See LICENSE file for license details


module Main(main,parseProgram,parseTest,parseFile) where


import Data.ByteString.Char8(split,pack,unpack,intercalate,readInt)
import Data.List
import qualified Data.Map as M
import Data.Ord
import Data.Char

import System.Console.GetOpt
import Data.Maybe ( fromMaybe )

import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.State

import System.Directory
import System.Environment

import HJS.Parser
import HJS.Parser.JavaScript

import HJS.Interpreter.InterpMDecl
import HJS.Interpreter.InterpM hiding (getArgs)
import HJS.Interpreter

data VFlag = Quiet | ShowProgress | ShowErrors | ShowAST deriving (Show,Eq)



-- Parse a file with supplied flags
parseFile :: [VFlag] -> String -> IO ()
parseFile flags fname = do
                          ifM (elem ShowProgress flags) (putStr $ "Parsing \"" ++ fname) (return ())
                          s <- readFile fname
                          handleResult flags $ parseProgram s

handleResult flags (Right r) | (elem ShowProgress flags) = putStrLn ("\"  ok")
                             | (elem ShowAST flags)      = putStrLn ("\" ok " ++ show r)
                             | otherwise = return ()
handleResult flags (Left r)  | (elem ShowProgress flags) = putStrLn ("\"  failed")
                             | (elem ShowErrors flags)   = putStrLn  ("\" failed" ++ show r)
                             | otherwise = return ()




runFile flags fname = do
               s <- readFile fname
               runString flags  s >>= putStrLn . show

runString :: [RunFlag] -> String -> IO Bool
runString flags s = do
                 case parseProgram s of
                        Right r -> runProgram flags   r
                        Left s -> putStrLn (show s) >> return False

runTest = testFiles "c:/Mark/MyDevelopments/haskell/HJS/hjs-0.2/testsuite" (runFile [])


main = do
         s <- getArgs
	 main' s

main' args = do
         (opt, files) <- interpOpts args
         case (foldr (\x s -> case x of Version -> (True || s); _ -> s) False opt) of
             True -> putStrLn "HJS - JavaScript Parser - Version 0.1"
             _ -> return ()
         let vf = foldr (\x s -> case x of (Verbose f) -> (f:s);_ -> s) [] opt
         mapM_ (runFile []) files


parseTest = do
	     -- testFiles "c:/Mark/MyDevelopments/haskell/HJS/hjs-0.2/testsuite" (parseFile [ShowProgress])
	     testFiles "c:/Mark/MyDevelopments/haskell/HJS/hjs-0.2/testsuite/parsingonly" (parseFile [ShowProgress])




testFiles dir action =   do fileList <- getTestFiles dir
			    mapM_ action  fileList



getTestFiles dirName  = do
          dirList <- getDirectoryContents dirName
          return $  map (\f -> dirName ++ "/" ++ f) $
                    map unpack $
	            map (Data.ByteString.Char8.intercalate (pack "_"))  $
		    sortBy (comparing (readInt . head)) $
		    filter (\l -> (<=) 2 (length l)) $
		    map (split '_' . pack) $
	            filter (\x -> last x /= '~' || head x == '.' ) dirList




data Flag  = Verbose VFlag | Version
		 deriving Show

options :: [OptDescr Flag]
options =
     [ Option ['v']     ["verbose"] (OptArg vflag "LEVEL" )       "=1 show progrss, =2 show errors =3 show AST"
     , Option ['V','?'] ["version"] (NoArg Version)       "show version number"
     ]

vflag :: Maybe String -> Flag
vflag Nothing = Verbose Quiet
vflag (Just s) = let s' = filter (\c -> not $ isSpace c) s
                 in case s' of
                          "1" -> Verbose ShowProgress
   		          "2" -> Verbose ShowErrors
                          "3" -> Verbose ShowAST
                          _ -> error $ "Cannot parse verbosity option " ++ s'

--inp,outp :: Maybe String -> Flag
--outp = Output . fromMaybe "stdout"
--inp  = Input  . fromMaybe "stdin"

interpOpts :: [String] -> IO ([Flag], [String])
interpOpts argv =
       case getOpt Permute options argv of
          (o,n,[]  ) -> return (o,n)
          (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
      where header = "Usage: hjs [OPTION...] files..."


ifM a b c = do
	        case a of
		   True -> b
                   False -> c