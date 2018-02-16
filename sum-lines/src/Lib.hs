module Lib
    ( doubleFromLineC
    , doubleFromFileC
    ) where

import Conduit (ConduitM, decodeUtf8C, lineC, peekForeverE, yield)
import Control.Monad.Trans.Resource (MonadResource)
import Data.Conduit ((.|), await)
import Data.Conduit.Binary (sourceFile)
import Data.Text (Text, unpack)
import System.FilePath.Posix (FilePath)

doubleFromLineC :: Monad m => ConduitM Text Double m ()
doubleFromLineC = peekForeverE (lineC yieldNumber)
  where
    yieldNumber = do
      mx <- await
      case mx of
        Just s -> yield $ convertToNumber s
        _ -> return ()

    convertToNumber :: Text -> Double
    convertToNumber s =
      let y = reads (unpack s) :: [(Double, String)]
      in case y of
        (f, _):_ -> f
        _ -> 0

doubleFromFileC :: (MonadResource m) => FilePath -> ConduitM () Double m ()
doubleFromFileC filePath = sourceFile filePath .| decodeUtf8C .| doubleFromLineC
