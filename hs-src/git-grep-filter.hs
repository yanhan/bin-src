{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

import Shelly
    ( Sh, cmd, echo_err, errExit, escaping, exit, fromText, liftIO, rm_f
    , runHandle, shelly, verbosely
    )
import qualified Data.Text as T
import qualified Data.Text.IO as TIO (hGetLine, hPutStrLn)
import System.Environment (getArgs, getProgName)
import System.IO (Handle, hClose, openTempFile)
import System.IO.Error (IOError)
import qualified System.Process as Proc (system)
import Control.Exception (try)
import BinSrc.GitUtils (truncate_colorized_line)

default (T.Text)

main :: IO ()
main = shelly $ verbosely $ do
  progName <- liftIO getProgName
  cmdArgs <- liftIO getArgs
  case cmdArgs of
    (grepStr:filterList) -> do
      (sysfpGitGrepOutFname, fh) <- liftIO $ openTempFile "/tmp" "ggfoutput"
      -- close the file handles since we dont use them
      liftIO $ hClose fh
      let gitGrepOutFname = T.pack sysfpGitGrepOutFname
      let gitGrepOpts = T.concat [ "grep -n --no-color '", T.pack grepStr, "'"]
      let grepVCmds = construct_grep_inverse_cmd filterList
      let gitGrepCmd = T.concat [gitGrepOpts, grepVCmds, "> ", gitGrepOutFname]
      -- ignore non-zero exit code for git-grep, since a git-grep yielding no
      -- matches will have a nonzero exit code
      _ <- errExit False $ escaping False $ cmd "git" gitGrepCmd
      -- Use grep to colorize the text and truncate lines to fit within terminal
      (sysfpGrepColorFname, fh2) <- liftIO $ openTempFile "/tmp" "ggfoutput"
      let grepColorOutFname = T.pack sysfpGrepColorFname
      let grepColorOpts = [ "--color=always ", T.pack ("'" ++ grepStr ++ "'")
                          , gitGrepOutFname
                          ]
      errExit False $ escaping False $
        runHandle "grep" grepColorOpts (grep_line_truncate fh2)
      liftIO $ hClose fh2
      _ <- liftIO $ Proc.system ("cat " ++ sysfpGrepColorFname ++ " | less -R")
      rm_f $ fromText gitGrepOutFname
      rm_f $ fromText grepColorOutFname
      exit 0
    [] -> do
      echo_err $ T.pack (progName ++ ": please supply a pattern for git-grep")
      exit 1

grep_line_truncate :: Handle -> Handle -> Sh ()
grep_line_truncate tmpfh stdoutHandle =
  liftIO process_line
  where
    process_line :: IO ()
    process_line = do
      eitherLine <- try (TIO.hGetLine stdoutHandle) :: IO (Either IOError T.Text)
      case eitherLine of
        Left _  -> return ()
        Right s -> do
          TIO.hPutStrLn tmpfh $ truncate_colorized_line 80 s
          process_line

-- given a list of strings to filter off, construct the appropriate series of
-- inverse grep (grep -v) to filter them off.
--
-- Eg. given ["cat", "dog", "hamster"], returns
-- " | grep -v 'cat' | grep -v 'dog' | grep -v 'hamster'"
construct_grep_inverse_cmd :: [String] -> T.Text
construct_grep_inverse_cmd =
  T.concat . map (\s -> T.pack $ " | grep -v '" ++ s ++ "'")
