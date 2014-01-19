{-# LANGUAGE OverloadedStrings #-}

module BinSrc.GitUtils (
  in_git_repo
, truncate_colorized_line
, truncate_colorized_line_io
) where

import qualified Data.Text as T
  ( Text, append, breakOn, concat, drop, length, null, take
  )
import qualified Data.Text.ICU.Regex as ICU
  ( Regex, end, find, regex', setText, start
  )
import Shelly (Sh, errExit, lastExitCode, run_, shelly)

-- only works for non bare repositories
in_git_repo :: Sh Bool
in_git_repo = shelly $ do
  errExit False $ run_ "git" ["rev-parse"]
  exitCode <- lastExitCode
  return $ exitCode == 0

colorEscapeStart :: T.Text
colorEscapeStart = "\ESC[01;31m\ESC[K"

colorEscapeStartLen :: Int
colorEscapeStartLen = 11

colorEscapeEnd :: T.Text
colorEscapeEnd = "\ESC[m\ESC[K"

colorEscapeEndLen :: Int
colorEscapeEndLen = 6

truncate_colorized_line :: Int -> T.Text -> T.Text
truncate_colorized_line maxCols line =
  let textList = truncate_helper 0 line []
  in
    T.concat $ reverse textList
  where
    truncate_helper :: Int -> T.Text -> [T.Text] -> [T.Text]
    truncate_helper accumLen curLine textList =
      let (prefix, suffix) = T.breakOn colorEscapeStart curLine
          prefixLen = T.length prefix
          accumLenWithPrefix = accumLen + prefixLen
          textListWithPrefix = prefix : textList
      in
        case (compare accumLenWithPrefix maxCols, T.null suffix) of
          (EQ, _) -> textListWithPrefix
          (GT, _) -> (T.take (maxCols - accumLen) prefix) : textList
          (LT, True) -> textListWithPrefix
          (LT, False) ->
            let textListWithColorStart = colorEscapeStart : textListWithPrefix
                curLineAfterEscapeStart = T.drop colorEscapeStartLen suffix
                (coloredText, coloredTextSuffix) =
                  T.breakOn colorEscapeEnd curLineAfterEscapeStart
                coloredTextLen = T.length coloredText
                finalAccumLen = accumLenWithPrefix + coloredTextLen
                remLine = T.drop colorEscapeEndLen coloredTextSuffix
            in
              case compare finalAccumLen maxCols of
                LT -> truncate_helper finalAccumLen remLine
                        (colorEscapeEnd : coloredText : textListWithColorStart)
                EQ -> colorEscapeEnd : coloredText : textListWithColorStart
                GT ->
                  let takeLen = maxCols - accumLenWithPrefix
                      coloredTextPartial = T.take takeLen coloredText
                  in
                    colorEscapeEnd : coloredTextPartial : textListWithColorStart

truncate_colorized_line_io :: Int -> T.Text -> IO T.Text
truncate_colorized_line_io maxCols line = do
  eitherErrRegex <- ICU.regex' [] "\\e\\[[^m]*m\\e\\[K([^\\e]*)\\e\\[[^m]*m\\e\\[K"
  case eitherErrRegex of
    Left err -> do
      putStrLn $ "regex parse error " ++ show err
      return line
    Right colorRegex -> do
      ICU.setText colorRegex line
      (takeTillLen, appendColorEscapeEnd) <- truncate_helper_io (0, 0) colorRegex
      let truncatedLine = T.take takeTillLen line
      if appendColorEscapeEnd
        then
          return $ T.append truncatedLine colorEscapeEnd
        else
          return truncatedLine

  where
    lineLen :: Int
    lineLen = T.length line

    truncate_helper_io :: (Int, Int) -> ICU.Regex -> IO (Int, Bool)
    truncate_helper_io (startIdx, accumLen) colorRegex = do
      foundPattern <- ICU.find colorRegex (fromIntegral startIdx)
      if foundPattern
        then do
          mbI16GZeroStart <- ICU.start colorRegex 0
          mbI16GZeroEnd <- ICU.end colorRegex 0
          mbI16GOneStart <- ICU.start colorRegex 1
          mbI16GOneEnd <- ICU.end colorRegex 1
          case (mbI16GZeroStart, mbI16GZeroEnd, mbI16GOneStart, mbI16GOneEnd) of
            (Just i16GZeroStart, Just i16GZeroEnd, Just i16GOneStart,
             Just i16GOneEnd) ->
              let gZeroStart = fromIntegral i16GZeroStart :: Int
                  gZeroEnd = fromIntegral i16GZeroEnd :: Int
                  gOneStart = fromIntegral i16GOneStart :: Int
                  gOneEnd = fromIntegral i16GOneEnd :: Int
                  coloredTextLen = gOneEnd - gOneStart
                  prefixLen = gZeroStart - startIdx
                  newAccumLen = accumLen + prefixLen
                  finalAccumLen = newAccumLen + coloredTextLen
              in
                case compare newAccumLen maxCols of
                  EQ -> return (gOneStart, False)
                  GT -> return (startIdx + maxCols - accumLen, False)
                  LT ->
                    case compare finalAccumLen maxCols of
                      LT -> truncate_helper_io (gZeroEnd, finalAccumLen) colorRegex
                      EQ -> return (gZeroEnd, False)
                      GT ->
                        let nrColorCharsToTake = maxCols - newAccumLen
                        in
                          return (gOneStart + nrColorCharsToTake, True)

            _ -> return (min lineLen maxCols, False)
        else
          if lineLen <= maxCols
            then
              return (lineLen, False)
            else
              let charsRemaining = maxCols - accumLen
              in
                return (startIdx + charsRemaining, False)
