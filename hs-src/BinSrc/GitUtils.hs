{-# LANGUAGE OverloadedStrings #-}

module BinSrc.GitUtils (
  truncate_colorized_line
) where

import Data.Text (Text)

truncate_colorized_line :: Int -> Text -> Text
truncate_colorized_line maxCols s = s
