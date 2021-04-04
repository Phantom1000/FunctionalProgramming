-- | Run Haskell tr implementation.
--
-- We will be testing using your `Tr` module, not `Main`, so don't worry too
-- much about compatibility of argument parsing.
module Main where

import Tr
import System.Console.GetOpt
import System.Environment

-- | Main - parse args, and read from stdin.
data Options = Options { deleteString :: Maybe String }

defaultOptions :: Options
defaultOptions = Options { deleteString = Nothing }

options :: [OptDescr (Options -> Options)]
options =
    [ Option "d" ["delete"]
        (ReqArg
            (\arg opt -> opt { deleteString = Just arg })
            "PROHIBITED CHARS")
        "Delete mode"  
    ]

compilerOpts :: [String] -> IO (Options, [String])
compilerOpts argv =
    case getOpt Permute options argv of
        (o,n,[]  ) -> pure (foldl (flip id) defaultOptions o, n)
        (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
    where header = "Usage: ic [OPTION...] files..."

main :: IO ()
main = do
    (opts, args) <- compilerOpts =<< getArgs
    let result = 
            case (deleteString opts, args) of
                (Just prohibitedChars, [input]) -> tr prohibitedChars Nothing input
                (Nothing, [inset, outset, input]) -> tr inset (Just outset) input
                _ -> ""
    putStrLn result

--main = do
    --putStrLn "hello"
    --putStrLn "world"

