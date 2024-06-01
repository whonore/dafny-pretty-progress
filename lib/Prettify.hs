{-# LANGUAGE OverloadedStrings #-}

module Prettify (prettify) where

import Control.Monad.State
import qualified Data.Map as M
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Read (decimal)
import qualified System.Console.ANSI as ANSI
import Text.Printf
import Text.Regex.TDFA ((=~))

data Progress = Progress {pNumerator :: Int, pDenominator :: Int}

progressToFractional :: (Fractional a) => Progress -> a
progressToFractional (Progress num den) = fromIntegral num / fromIntegral den

data Summary = Summary {sDuration :: Int, sResource :: Double}

data VerifiedState = VerifiedState
  { vsProgress :: Progress,
    vsWorking :: M.Map T.Text Progress,
    vsComplete :: M.Map T.Text Summary
  }

initState :: VerifiedState
initState = VerifiedState (Progress 0 1) M.empty M.empty

verifiedSymbol :: T.Text
verifiedSymbol = "Verified ([0-9]+)/([0-9]+) symbols\\. Waiting for (.+) to verify\\."

verifiedPart :: T.Text
verifiedPart = "Verified part ([0-9]+)/([0-9]+) of (.+), on line [0-9]+ \\(time: ([0-9]+ms), resource count: ([0-9E+.]+)\\)"

parseVerifiedSymbol :: T.Text -> Maybe (Int, Int, T.Text)
parseVerifiedSymbol line =
  case matches of
    [curSymbol, totalSymbol, next] ->
      either (const Nothing) Just $ do
        (curSymbol', _) <- decimal curSymbol
        (totalSymbol', _) <- decimal totalSymbol
        return (curSymbol', totalSymbol', next)
    _ -> Nothing
  where
    (_, _, _, matches) = line =~ verifiedSymbol :: (T.Text, T.Text, T.Text, [T.Text])

updateVerifiedSymbol :: T.Text -> State VerifiedState ()
updateVerifiedSymbol = maybe (return ()) update . parseVerifiedSymbol
  where
    update :: (Int, Int, T.Text) -> State VerifiedState ()
    update (curSymbol, totalSymbol, next) = do
      let progress = Progress curSymbol totalSymbol
      working <- gets vsWorking
      complete <- gets vsComplete
      let working' =
            if M.notMember next working && M.notMember next complete
              then M.insert next (Progress 0 1) working
              else working
      modify (\st -> st {vsProgress = progress, vsWorking = working'})

parseVerifiedPart :: T.Text -> Maybe (Int, Int, T.Text)
parseVerifiedPart line =
  case matches of
    [curPart, totalPart, next, _, _] ->
      either (const Nothing) Just $ do
        (curPart', _) <- decimal curPart
        (totalPart', _) <- decimal totalPart
        return (curPart', totalPart', next)
    _ -> Nothing
  where
    (_, _, _, matches) = line =~ verifiedPart :: (T.Text, T.Text, T.Text, [T.Text])

updateVerifiedPart :: T.Text -> State VerifiedState ()
updateVerifiedPart = maybe (return ()) update . parseVerifiedPart
  where
    update :: (Int, Int, T.Text) -> State VerifiedState ()
    update (curPart, totalPart, next) = do
      let progress = Progress curPart totalPart
      let done = curPart == totalPart
      working <- gets vsWorking
      complete <- gets vsComplete
      let working' = if not done then M.insert next progress working else M.delete next working
      -- TODO: actually parse the time and resource usage
      let complete' = if done then M.insert next (Summary 0 0) complete else complete
      modify (\st -> st {vsWorking = working', vsComplete = complete'})

updateState :: T.Text -> State VerifiedState ()
updateState line = do
  updateVerifiedSymbol line
  updateVerifiedPart line

showState :: Int -> State VerifiedState T.Text
showState tRows = do
  VerifiedState {vsProgress = progress, vsWorking = working, vsComplete = complete} <- get
  return $
    prelude
      <> showTotal progress
      <> divider "Verifying"
      <> T.intercalate "\n" (showWorking working)
      <> divider "Completed"
      -- TODO: maybe show only most recent completed entries?
      <> T.intercalate "\n" (take (maxComplete working) $ showComplete complete)
  where
    maxComplete working = tRows - M.size working - 3
    prelude = T.pack $ ANSI.clearScreenCode ++ ANSI.setCursorPositionCode 0 0
    divider title =
      boxStyle $
        T.concat
          [ "\n",
            "├",
            T.replicate 10 "─",
            title,
            T.replicate 10 "─",
            "\n"
          ]
    showTotal progress =
      T.concat
        [ boxStyle "┌ ",
          symbolStyle "Progress",
          boxStyle " ─ ",
          showProgress progress
        ]
    showProgress progress =
      progressStyle . T.pack $
        printf
          "%.2f%% (%d/%d)"
          (100 * progressToFractional progress :: Double)
          (pNumerator progress)
          (pDenominator progress)
    showSummary summary =
      summaryStyle . T.pack $ printf "%dms (%.2f)" (sDuration summary) (sResource summary)
    showWorking =
      map
        ( \(symbol, progress) ->
            T.concat
              [ boxStyle "├ ",
                symbolStyle symbol,
                boxStyle " ─ ",
                showProgress progress
              ]
        )
        . M.toList
    showComplete =
      map
        ( \(symbol, summary) ->
            T.concat
              [ boxStyle "├ ",
                symbolStyle symbol,
                boxStyle " ─ ",
                showSummary summary
              ]
        )
        . M.toList
    withColor color intensity txt =
      T.concat
        [ T.pack $ ANSI.setSGRCode [ANSI.SetColor ANSI.Foreground intensity color],
          txt,
          T.pack $ ANSI.setSGRCode [ANSI.Reset]
        ]
    boxStyle = withColor ANSI.Green ANSI.Vivid
    progressStyle = withColor ANSI.Yellow ANSI.Vivid
    summaryStyle = withColor ANSI.Yellow ANSI.Vivid
    symbolStyle = withColor ANSI.Cyan ANSI.Vivid

prettify :: [T.Text] -> [T.Text]
prettify = flip evalState initState . mapM (\line -> updateState line >> showState tRows)
  where
    tRows = 40 -- TODO: get the actual terminal size
