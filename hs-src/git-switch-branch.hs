{-# LANGUAGE OverloadedStrings #-}

import Data.Text (Text)
import qualified Data.Text as T (append, concat, drop, head, length, pack)
import qualified Data.Text.IO as TIO (getLine, putStr, putStrLn)
import Data.Text.Read (decimal)
import System.IO (hFlush, stdout)
import Shelly (Sh, echo, exit, liftIO, run_, runFoldLines, shelly, silently)
import BinSrc.GitUtils (exit_if_not_in_git_repo)

main :: IO ()
main = shelly $ do
  exit_if_not_in_git_repo
  (nrBranches, curBranchIndex, gitBranches) <- silently get_git_branches
  let maxDigits = compute_nr_digits (nrBranches - 1)
  eitherExitBranchIdx <- liftIO $ ask_branch_from_user
                                    ( nrBranches, maxDigits, curBranchIndex
                                    , gitBranches
                                    )
  case eitherExitBranchIdx of
    Left _                  -> exit 0
    Right branchIndexChosen ->
      let branchChosen = gitBranches !! branchIndexChosen
      in
        if branchIndexChosen == curBranchIndex
          then do
            echo $ T.concat ["Already on branch ", branchChosen, ". Exiting."]
            exit 0 
          else do
            echo $ T.append "Checking out branch " branchChosen
            run_ "git" ["checkout", branchChosen]
  where
    get_git_branches :: Sh (Int, Int, [Text])
    get_git_branches = runFoldLines (0, -1, []) remove_git_branch_output_prefix
                         "git" ["branch", "--no-color", "--no-abbrev"]
    -- assume we have only a few branches, so ignore time complexity of
    -- list concatenation
    -- Returns (number of branches, index of current branch, branch names)
    remove_git_branch_output_prefix :: (Int, Int, [Text]) -> Text ->
                                       (Int, Int, [Text])
    remove_git_branch_output_prefix (index, curBranchIndex, branchList) line
      | curBranchIndex == (-1) && T.head line == '*' =
          (nextIndex, index, newBranchList)
      | otherwise                                    =
          (nextIndex, curBranchIndex, newBranchList)
      where
        nextIndex = index + 1
        newBranchList = branchList ++ [T.drop 2 line]

compute_nr_digits :: Int -> Int
compute_nr_digits 0 = 1
compute_nr_digits n
  | n < 0 = 1 + compute_nr_digits (-n)
  | otherwise = 1 + compute_nr_digits (n `div` 10)

ask_branch_from_user :: (Int, Int, Int, [Text]) -> IO (Either Int Int)
ask_branch_from_user (nrBranches, maxDigits, curBranchIndex, gitBranches) =
    helper
  where
    helper :: IO (Either Int Int)
    helper = do
      TIO.putStrLn "Which branch do you want to switch to?"
      mapM_ TIO.putStrLn gitBranchesProcessed
      TIO.putStr "Enter branch number (Press enter to quit): "
      hFlush stdout
      line <- TIO.getLine
      if T.length line == 0
        then
          return $ Left 0
        else
          case (decimal line) of
            Left _ ->
              TIO.putStrLn "Invalid input. Please try again.\n" >>
              helper
            Right (branchNum, _) ->
              if branchNum >= 0 && branchNum < nrBranches
                then
                  return $ Right branchNum
                else do
                  TIO.putStrLn $ out_of_bounds_msg branchNum
                  TIO.putStrLn "Please try again.\n"
                  helper

    gitBranchesProcessed :: [Text]
    gitBranchesProcessed =
      snd $
        foldr (\line (idx, lst) ->
                let idxText = T.pack $ show idx
                    idxTextLen = T.length idxText
                    padding = max 0 (maxDigits - idxTextLen)
                    extraSpaces = T.pack $ replicate padding ' '
                    branchLine = T.concat [ "[", idxText, "] ", extraSpaces
                                          , line
                                          ]
                    nextIdx = idx - 1
                in
                  if idx == curBranchIndex
                    then
                      (nextIdx, T.append branchLine " <-- Current branch" : lst)
                    else
                      (nextIdx, branchLine : lst)
              ) (nrBranches-1, []) gitBranches

    maxBranchIndexText :: Text
    maxBranchIndexText = T.pack $ show (nrBranches - 1)

    out_of_bounds_msg :: Int -> Text
    out_of_bounds_msg idx =
      T.concat [ "Branch index ", T.pack (show idx)
               , " out of bounds [must be from 0 to ", maxBranchIndexText, "]"
               ]
