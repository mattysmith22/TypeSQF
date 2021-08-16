{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}

module Preproc
    ( preprocess, buildPreprocStream, traverseStream, PreprocStream(..)
    ) where

import System.Process (readProcessWithExitCode)
import System.IO (FilePath)
import System.Exit (ExitCode(ExitSuccess))
import Text.Megaparsec.Stream
import Data.Proxy
import Data.Char (isSpace, isDigit)
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Maybe ( fromMaybe, isJust )
import Data.List
import Text.Megaparsec
import Text.Megaparsec.Pos
import qualified Control.Monad.Trans.State as StateM
import GHC.RTS.Flags (MiscFlags(installSEHHandlers))

preprocess :: FilePath -> IO (Either String String)
preprocess path = do
    (exitCode, stdOut, stdErr) <- readProcessWithExitCode "sqfvm/sqfvm" ["-a", "--preprocess-file", path, "--parse-only"] ""
    if null stdErr then
        return $ Right stdOut
    else
        return $ Left (stdOut ++ stdErr)

data PreprocStream = PreprocStream String Bool
    deriving (Show, Eq)

buildPreprocStream :: String -> PreprocStream
buildPreprocStream xs = PreprocStream xs True

type CharEnteredCallback b = Char -> b -> b
type LineMacroReadCallback b = Int -> String -> b -> b
type ShouldStop b = Char -> b -> Bool

deleteIfAtStart :: Char -> String -> String
deleteIfAtStart _ "" = ""
deleteIfAtStart c s@(x:xs)
    | c == x = xs
    | otherwise = s

parseLineString :: String -> ((Int, FilePath), String )
parseLineString = StateM.runState $ do
      dropSpaces
      number <- fmap (+1) readNumber
      dropSpaces
      filePath <- readUntilNewline
      dropNewline
      return (number, filePath)
    where
      dropSpaces = StateM.state (\s -> ((), dropWhile isSpace s))
      readNumber = StateM.state (\s -> let
          (number, remainder) = break (not . isDigit) s
        in ((read::String -> Int) number, remainder))
      readUntilNewline = StateM.state (break (=='\n'))
      dropNewline = StateM.state (\case
        [] -> ((), [])
        (_:xs) -> ((), xs))

traverseStream :: CharEnteredCallback b -> LineMacroReadCallback b -> ShouldStop b -> b -> PreprocStream -> (b, PreprocStream)
traverseStream charEntered lineRead shouldStop init s@(PreprocStream string lastWasNewline)
    | lastWasNewline && ("#line" `isPrefixOf` string) =
        let string' = drop 5 string
            ((line, file), string'') = parseLineString string'
        in traverseStream' (lineRead line file init) (PreprocStream string'' True)
    | otherwise =
        case string of
            (x:xs) -> if shouldStop x init then
                (init, s)
            else
                traverseStream' (charEntered x init) (PreprocStream xs (x == '\n'))
            [] -> (init, s)
    where
        traverseStream' = traverseStream charEntered lineRead shouldStop

instance Stream PreprocStream where
    type Token PreprocStream = Char 
    type Tokens PreprocStream = String

    tokenToChunk pxy tok = tokensToChunk pxy [tok]
    tokensToChunk pxy = id
    chunkToTokens pxy = id
    chunkLength pxy = length
    chunkEmpty pxy = null

    take1_ stream = case traverseStream (\c _ -> Just c) (const $ const id) (const isJust) Nothing stream of
        (Nothing, _) -> Nothing
        (Just x, stream') -> Just (x, stream')

    takeN_ n stream
      | n <= 0 = Just ("", stream)
      | otherwise = case traverseStream charEntered lineMacroRead shouldStop initState stream of
              ((str, _), stream') -> 
                let result = str ""
                in if null result then
                  Nothing
                else
                  Just (result, stream')
          where
              charEntered c (str, n) = (str.(c:), n+1)
              lineMacroRead = const $ const id
              shouldStop _ (_,n') = n' >= n
              initState = (id, 0)
    
    takeWhile_ p stream = case traverseStream charEntered lineMacroRead shouldStop initState stream of
            (str, stream') -> (str "", stream')
        where
            charEntered c str = if p c then
                str.(c:)
              else
                str
            lineMacroRead = const $ const id
            shouldStop c _ = not (p c)
            initState = id
data St = St SourcePos ShowS

-- | Replace tab characters with given number of spaces.
expandTab ::
  Pos ->
  String ->
  String
expandTab w' = go 0
  where
    go 0 [] = []
    go 0 ('\t' : xs) = go w xs
    go 0 (x : xs) = x : go 0 xs
    go n xs = ' ' : go (n - 1) xs
    w = unPos w'

-- Taken from megaparsec's VisualStream internals
-- | A helper definition to facilitate defining 'reachOffset' for various
-- stream types.
reachOffset' ::
  Int ->
  -- | Initial 'PosState' to use
  PosState PreprocStream ->
  -- | Line at which 'SourcePos' is located, updated 'PosState'
  (Maybe String, PosState PreprocStream)
reachOffset'
  targetOffset
  PosState {..} =
    ( Just $ case expandTab pstateTabWidth
        . addPrefix
        . f
        . fst
        $ takeWhile_ (/= '\n') state' of
        "" -> "<empty line>"
        xs -> xs,
      PosState
        { pstateInput = state',
          pstateOffset = max pstateOffset targetOffset,
          pstateSourcePos = spos,
          pstateTabWidth = pstateTabWidth,
          pstateLinePrefix =
            if sameLine
              then -- NOTE We don't use difference lists here because it's
              -- desirable for 'PosState' to be an instance of 'Eq' and
              -- 'Show'. So we just do appending here. Fortunately several
              -- parse errors on the same line should be relatively rare.
                pstateLinePrefix ++ f ""
              else f ""
        }
    )
    where
      addPrefix xs =
        if sameLine
          then pstateLinePrefix ++ xs
          else xs
      sameLine = sourceLine spos == sourceLine pstateSourcePos
      ((St spos f, offset'), state') = traverseStream charEntered lineRead shouldStop initState pstateInput
      lineRead line file (_, offset) = (St (SourcePos file (mkPos line) pos1) id, offset)
      charEntered ch (St apos g, offset) =
        let SourcePos n l c = apos
            c' = unPos c
            w = unPos pstateTabWidth
         in if
                | ch == '\n' ->
                  (St
                    (SourcePos n (l <> pos1) pos1)
                    id, offset+1)
                | ch == '\t' ->
                  (St
                    (SourcePos n l (mkPos $ c' + w - ((c' - 1) `rem` w)))
                    (g . (ch :)), offset+1)
                | otherwise ->
                  (St
                    (SourcePos n l (c <> pos1))
                    (g . (ch :)), offset+1)
      shouldStop ch (_, offset) = offset >= targetOffset
      initState = (St pstateSourcePos id, pstateOffset)

-- | Like 'reachOffset'' but for 'reachOffsetNoLine'.
reachOffsetNoLine' ::
  Int ->
  PosState PreprocStream ->
  -- | Updated 'PosState'
  PosState PreprocStream
reachOffsetNoLine'
  targetOffset
  PosState {..} =
    ( PosState
        { pstateInput = post,
          pstateOffset = max pstateOffset targetOffset,
          pstateSourcePos = spos,
          pstateTabWidth = pstateTabWidth,
          pstateLinePrefix = pstateLinePrefix
        }
    )
    where
      ((spos, offset), post) = traverseStream charEntered lineRead shouldStop init pstateInput
      charEntered ch (SourcePos n l c, offset) = 
        let c' = unPos c
            w = unPos pstateTabWidth
        in if
            | ch == '\n' ->
              (SourcePos n (l <> pos1) pos1, offset+1)
            | ch == '\t' ->
              (SourcePos n l (mkPos $ c' + w - ((c' - 1) `rem` w)), offset+1)
            | otherwise ->
              (SourcePos n l (c <> pos1), offset+1)
      lineRead line file (_, offset) = (SourcePos file (mkPos line) pos1, offset)
      shouldStop _ (_, offset) = offset >= targetOffset
      init = (pstateSourcePos,pstateOffset)


preprocSplitAt :: Int -> PreprocStream -> (String, PreprocStream)
preprocSplitAt n stream = fromMaybe ("", stream) $ takeN_ n stream

instance VisualStream PreprocStream where
    showTokens pxy ts = showTokens (Proxy::Proxy String) ts

instance TraversableStream PreprocStream where
  reachOffset = reachOffset'
  reachOffsetNoLine = reachOffsetNoLine'