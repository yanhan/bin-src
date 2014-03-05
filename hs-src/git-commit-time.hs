{-# LANGUAGE OverloadedStrings #-}

import Data.Text (Text)
import qualified Data.Text as T (pack, unpack)
import Shelly
  ( Sh, liftIO, echo_err, errExit, exit, lastExitCode, run, shelly, silently
  )
import System.Environment (getArgs)
import BinSrc.GitUtils (exit_if_not_in_git_repo)

default (Text)

main :: IO ()
main = shelly $ silently $ do
  exit_if_not_in_git_repo
  cmdArgs <- liftIO getArgs
  case cmdArgs of
    [] -> do
      echo_err "Given a list of git commits, output their commit time"
      echo_err "Usage: <commit1> [commit2 commit3 ...]"
      exit 1
    _ -> do
      commitTimeList <- get_commit_time_list cmdArgs
      liftIO (
        mapM_ (\(commitSha1, mbCommitTime) ->
          case mbCommitTime of
            Just commitTime ->
              putStr $ "commit " ++ commitSha1 ++ " was made on "
                ++ T.unpack commitTime
            _ ->
              putStrLn $ commitSha1 ++ " is not a commit"
         ) $ zip cmdArgs commitTimeList
       )
  return ()

get_commit_time_list :: [String] -> Sh [Maybe Text]
get_commit_time_list commitSha1List =
  mapM (\commitSha1 -> shelly $ errExit False $ silently $ do
    outText <- run "git" ["show", "-s", "--format=%ci", T.pack commitSha1]
    exitCode <- lastExitCode
    if exitCode == 0
      then
        return $ Just outText
      else
        return Nothing
  ) commitSha1List
