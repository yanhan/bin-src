{-# LANGUAGE OverloadedStrings #-}

import Data.Text (Text)
import qualified Data.Text as T (pack)
import Shelly
  ( Sh, StdHandle(InHandle, OutHandle, ErrorHandle)
  , StdStream(CreatePipe, Inherit, UseHandle), liftIO, echo_err, errExit, exit
  , lastExitCode, runHandles, shelly, silently
  )
import System.Environment (getArgs)
import System.IO
  ( Handle, IOMode(WriteMode), hClose, hGetContents, hPutStr, openFile
  )
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
                ++ commitTime
            _ ->
              putStrLn $ commitSha1 ++ " is not a commit"
         ) $ zip cmdArgs commitTimeList
       )
  return ()

get_commit_time_list :: [String] -> Sh [Maybe String]
get_commit_time_list commitSha1List = do
  junkOutputHandle <- liftIO $ openFile "/dev/null" WriteMode
  ret <- mapM (get_commit_time junkOutputHandle) commitSha1List
  liftIO $ hClose junkOutputHandle
  return ret
  where
    get_commit_time :: Handle -> String -> Sh (Maybe String)
    get_commit_time junkOutputHandle commitSha1 =
      shelly $ errExit False $ silently $ do
        devNullHandle <- liftIO $ openFile "/dev/null" WriteMode
        let devNullStdStream = UseHandle devNullHandle
        outText <- runHandles "git" [ "show", "-s", "--format=%ci"
                                    , T.pack commitSha1
                                    ]
          [ InHandle Inherit, OutHandle CreatePipe
          , ErrorHandle devNullStdStream
          ]
          (\_ outHand _ -> do
            t <- liftIO $ hGetContents outHand
            -- force evaluation of hGetContents above
            liftIO $ hPutStr junkOutputHandle t
            return t
          )
        exitCode <- lastExitCode
        liftIO $ hClose devNullHandle
        if exitCode == 0
          then
            return $ Just outText
          else do
            return Nothing
