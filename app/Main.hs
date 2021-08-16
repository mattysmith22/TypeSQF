module Main where

import Preproc
import System.Environment
import Text.Megaparsec
import Control.Monad.Combinators
import Data.Void
import Text.Megaparsec.Error (errorBundlePretty)

readStream :: Parsec Void PreprocStream [(Char, SourcePos)]
readStream = many $ do
    char <- anySingle
    pos <- getSourcePos
    return (char, pos)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> putStrLn "Error: Requires File Path"
        (filePath:_) -> do
            preprocessResult <- preprocess filePath
            case preprocessResult of
                (Left err) -> do
                    putStrLn "Error"
                    print err
                (Right text) -> do
                    putStrLn "Success, resultant text:"
                    putStrLn text
                    putStrLn "Stream output"
                    let result = runParser readStream "" (buildPreprocStream text)
                    case result of
                        (Left err) ->
                            putStrLn $ errorBundlePretty err
                        (Right val) ->
                            mapM_ print val