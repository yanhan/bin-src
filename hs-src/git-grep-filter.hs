{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

import Shelly
    ( cmd, echo_err, errExit, escaping, exit, fromText, liftIO, rm_f, shelly
    , verbosely
    )
import qualified Data.Text as T
import System.Environment (getArgs, getProgName)
import System.IO (hClose, openTempFile)
import qualified System.Process as Proc (system)

default (T.Text)

main :: IO ()
main = shelly $ verbosely $ do
  progName <- liftIO getProgName
  cmdArgs <- liftIO getArgs
  case cmdArgs of
    (grepStr:filterList) -> do
      (sysfpGitGrepOutFname, fh) <- liftIO $ openTempFile "/tmp" "ggfoutput"
      (sysfpGrepOutFname, fh2) <- liftIO $ openTempFile "/tmp" "ggfoutput"
      -- close the file handles since we dont use them
      liftIO $ hClose fh
      liftIO $ hClose fh2
      let gitGrepOutFname = T.pack sysfpGitGrepOutFname
      let grepOutFname = T.pack sysfpGrepOutFname
      let gitGrepOpts = T.concat [ "grep --no-color '", T.pack grepStr, "' > "
                                 , gitGrepOutFname
                                 ]
      -- ignore non-zero exit code for git-grep, since a git-grep yielding no
      -- matches will have a nonzero exit code
      _ <- errExit False $ escaping False $ cmd "git" gitGrepOpts
      -- filter out all the unwanted stuff, and redirect output to grepOutFname
      let grepVCmds = construct_grep_inverse_cmd filterList
      let catGrepInvCmd = T.concat [ " ", gitGrepOutFname, grepVCmds
                                   , " > ", grepOutFname
                                   ]
      _ <- errExit False $ escaping False $ cmd "cat" catGrepInvCmd
      -- use `grep` to display the final output
      _ <- liftIO $ Proc.system ("grep --color=always '" ++ grepStr ++ "' " ++
                                 T.unpack grepOutFname ++ " | less -R"
                                )
      rm_f $ fromText gitGrepOutFname
      rm_f $ fromText grepOutFname
      exit 0
    [] -> do
      echo_err $ T.pack (progName ++ ": please supply a pattern for git-grep")
      exit 1

-- given a list of strings to filter off, construct the appropriate series of
-- inverse grep (grep -v) to filter them off.
--
-- Eg. given ["cat", "dog", "hamster"], returns
-- " | grep -v 'cat' | grep -v 'dog' | grep -v 'hamster'"
construct_grep_inverse_cmd :: [String] -> T.Text
construct_grep_inverse_cmd =
  T.concat . map (\s -> T.pack $ " | grep -v '" ++ s ++ "'")
