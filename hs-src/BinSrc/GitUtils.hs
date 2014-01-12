{-# LANGUAGE OverloadedStrings #-}

module BinSrc.GitUtils (
  truncate_colorized_line
) where

import qualified Data.Text as T
  ( Text, breakOn, concat, drop, length, null, take
  )

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
