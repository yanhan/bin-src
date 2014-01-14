{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

import Shelly
    ( Sh, cmd, echo_err, errExit, escaping, exit, fromText, liftIO, rm_f
    , runHandle, shelly, verbosely
    )
import qualified Data.Text as T
import qualified Data.Text.IO as TIO (hGetLine, hPutStrLn, putStrLn)
import Data.Sequence ((|>), ViewL((:<), EmptyL))
import qualified Data.Sequence as S (Seq, empty, viewl)
import System.Environment (getArgs, getProgName)
import System.IO (Handle, hClose, openTempFile)
import System.IO.Error (IOError)
import qualified System.Process as Proc (system)
import Control.Exception (try)
import BinSrc.GitUtils (truncate_colorized_line)

default (T.Text)

data GrepLine = BinaryLine T.Text
              | TextLine T.Text Int

main :: IO ()
main = shelly $ verbosely $ do
  progName <- liftIO getProgName
  cmdArgs <- liftIO getArgs
  case cmdArgs of
    (grepStr:filterList) -> do
      (sysfpGitGrepOutFname, fh) <- liftIO $ openTempFile "/tmp" "ggfoutput"
      -- close the file handles since we dont use them
      let gitGrepOutFname = T.pack sysfpGitGrepOutFname
      let gitGrepOpts = T.concat [ "grep -n --no-color '", T.pack grepStr, "'"]
      let grepVCmds = construct_grep_inverse_cmd filterList
      let gitGrepCmd = [gitGrepOpts, grepVCmds]
      -- ignore non-zero exit code for git-grep, since a git-grep yielding no
      -- matches will have a nonzero exit code
      prefixes <- errExit False $ escaping False $
                    runHandle "git" gitGrepCmd (remove_fileinfo_from_lines fh)
      liftIO $ hClose fh
      -- Use grep to colorize the text and truncate lines to fit within terminal
      (sysfpGrepColorFname, fh2) <- liftIO $ openTempFile "/tmp" "ggfoutput"
      let grepColorOutFname = T.pack sysfpGrepColorFname
      let grepColorOpts = [ "--color=always ", T.pack ("'" ++ grepStr ++ "'")
                          , gitGrepOutFname
                          ]
      errExit False $ escaping False $
        runHandle "grep" grepColorOpts (grep_line_truncate fh2 prefixes)
      liftIO $ hClose fh2
      _ <- liftIO $ Proc.system ("cat " ++ sysfpGrepColorFname ++ " | less -R")
      rm_f $ fromText gitGrepOutFname
      rm_f $ fromText grepColorOutFname
      exit 0
    [] -> do
      echo_err $ T.pack (progName ++ ": please supply a pattern for git-grep")
      exit 1

remove_fileinfo_from_lines :: Handle -> Handle -> Sh (S.Seq GrepLine)
remove_fileinfo_from_lines outFH stdoutHandle =
  liftIO $ process_line S.empty
  where
    eqColon :: Char -> Bool
    eqColon = (== ':')

    process_line :: S.Seq GrepLine -> IO (S.Seq GrepLine)
    process_line lst = do
      eitherLine <- try (TIO.hGetLine stdoutHandle) :: IO (Either IOError T.Text)
      case eitherLine of
        Left _ -> return lst
        Right s ->
          let (before1stColon, s') = T.break eqColon s
          in
            if T.null s'
              then
                -- Binary file
                process_line $ (|>) lst (BinaryLine s)
              else
                -- break again
                let (before2ndColon, s'') = T.break eqColon (T.tail s')
                    grepLinePrefix = T.concat [ before1stColon, ":"
                                              , before2ndColon, ":"
                                              ]
                    prefixLen = T.length grepLinePrefix
                in
                  -- write line to file
                  TIO.hPutStrLn outFH (T.tail s'') >>
                  (process_line $ (|>) lst (TextLine grepLinePrefix prefixLen))

grep_line_truncate :: Handle -> S.Seq GrepLine -> Handle -> Sh ()
grep_line_truncate tmpfh prefixes stdoutHandle =
  liftIO $ process_line prefixes
  where
    process_line :: S.Seq GrepLine -> IO ()
    process_line xs =
      case S.viewl xs of
        ((BinaryLine l) :< remXs) -> do
          TIO.hPutStrLn tmpfh l >> process_line remXs
        ((TextLine l len) :< remXs) -> do
          eitherLine <- try (TIO.hGetLine stdoutHandle) :: IO (Either IOError T.Text)
          case eitherLine of
            Left _  -> return ()
            Right s -> do
              -- TODO: Get the actual column width
              let toTruncate = max 0 (80 - len)
              let truncatedLine = truncate_colorized_line toTruncate s
              TIO.hPutStrLn tmpfh $ T.append l truncatedLine
              process_line remXs
        EmptyL -> return ()

-- given a list of strings to filter off, construct the appropriate series of
-- inverse grep (grep -v) to filter them off.
--
-- Eg. given ["cat", "dog", "hamster"], returns
-- " | grep -v 'cat' | grep -v 'dog' | grep -v 'hamster'"
construct_grep_inverse_cmd :: [String] -> T.Text
construct_grep_inverse_cmd =
  T.concat . map (\s -> T.pack $ " | grep -v '" ++ s ++ "'")
