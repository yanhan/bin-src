{-# LANGUAGE OverloadedStrings #-}

module BinSrc.GitUtils.Tests (
  tests
) where

import qualified Data.Text as T (length)
import BinSrc.GitUtils (truncate_colorized_line)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, (@=?))

tests :: Test
tests = testGroup "truncate_colorized_line"
    [ testCase "does not truncate a line shorter than the terminal width"
        assert_notruncate_lt_terminal_width
    , testCase "does not truncate a line equal to the terminal width"
        assert_notruncate_eq_terminal_width
    , testCase "truncates a line longer than the terminal width"
        assert_truncate_gt_terminal_width
    , testCase "does not truncate a colorized line shorter than terminal width"
        assert_no_truncate_color_lt_terminal_width
    , testCase "does not truncate a colorized line equal to the terminal width"
        assert_no_truncate_color_eq_terminal_width
    , testCase "truncates a colorized line longer than the terminal width"
        assert_truncate_color_gt_terminal_width
    , testCase "truncates a colorized line longer than terminal width (colored text after)"
        assert_truncate_color_gt_terminal_width_after
    , testCase "does not truncate a colorized line equal to the terminal width (right at color sequence)"
        assert_no_truncate_color_eq_terminal_width_at_sequence
    , testCase "truncates within color sequence when length exceeds terminal width, but appends closing color escape characters"
        assert_truncate_within_color_sequence_if_exceed
    ]

assert_notruncate_lt_terminal_width :: Assertion
assert_notruncate_lt_terminal_width =
  let orgText = "first test case"
      actual = truncate_colorized_line 70 orgText
  in
    orgText @=? actual

assert_notruncate_eq_terminal_width :: Assertion
assert_notruncate_eq_terminal_width =
  let orgText = "i hope this line is not truncated"
      orgTextLen = T.length orgText
      actual = truncate_colorized_line orgTextLen orgText
  in
    orgText @=? actual

assert_truncate_gt_terminal_width :: Assertion
assert_truncate_gt_terminal_width =
  let orgText = "this will be truncated, my friend"
      columnWidth = T.length orgText - 2
      actual = truncate_colorized_line columnWidth orgText
  in
    "this will be truncated, my frie" @=? actual

assert_no_truncate_color_lt_terminal_width :: Assertion
assert_no_truncate_color_lt_terminal_width =
  let orgText = "what is \ESC[01;31m\ESC[Kthe\ESC[m\ESC[K definition of \ESC[01;31m\ESC[Kpassing\ESC[m\ESC[K out?"
      actual = truncate_colorized_line 80 orgText
  in
    orgText @=? actual

assert_no_truncate_color_eq_terminal_width :: Assertion
assert_no_truncate_color_eq_terminal_width =
  let orgText = "well this is \ESC[01;31m\ESC[Ksomething\ESC[m\ESC[K we've been looking \ESC[01;31m\ESC[Kforward\ESC[m\ESC[K to"
      actual = truncate_colorized_line 52 orgText
  in
    orgText @=? actual

assert_truncate_color_gt_terminal_width :: Assertion
assert_truncate_color_gt_terminal_width =
  let orgText = "this \ESC[01;31m\ESC[Kshall\ESC[m\ESC[K be truncated \ESC[01;31m\ESC[Kat non colorized\ESC[m\ESC[K text"
      expected = "this \ESC[01;31m\ESC[Kshall\ESC[m\ESC[K be truncated \ESC[01;31m\ESC[Kat non colorized\ESC[m\ESC[K t"
      actual = truncate_colorized_line 42 orgText
  in
    expected @=? actual

assert_truncate_color_gt_terminal_width_after :: Assertion
assert_truncate_color_gt_terminal_width_after =
  let orgText = "who \ESC[01;31m\ESC[Kwhat when\ESC[m\ESC[K where \ESC[01;31m\ESC[Khow\ESC[m\ESC[K ? I have absolutely no idea what \ESC[01;31m\ESC[Kjust\ESC[m\ESC[K happened or what could \ESC[01;31m\ESC[Kpossibly\ESC[m\ESC[K happen"
      expected = "who \ESC[01;31m\ESC[Kwhat when\ESC[m\ESC[K where \ESC[01;31m\ESC[Khow\ESC[m\ESC[K ? I have absolutely no idea what \ESC[01;31m\ESC[Kjust\ESC[m\ESC[K happened or "
      actual = truncate_colorized_line 74 orgText
  in
    expected @=? actual

assert_no_truncate_color_eq_terminal_width_at_sequence :: Assertion
assert_no_truncate_color_eq_terminal_width_at_sequence =
  let orgText = "\ESC[01;31m\ESC[Khey wait up\ESC[m\ESC[K ! I am talking to \ESC[01;31m\ESC[Kyou\ESC[m\ESC[K"
      actual = truncate_colorized_line 33 orgText
  in
    orgText @=? actual

assert_truncate_within_color_sequence_if_exceed :: Assertion
assert_truncate_within_color_sequence_if_exceed =
  let orgText = "I have an apple, and \ESC[01;31m\ESC[Kyou\ESC[m\ESC[K have two. I have a bracelet and an \ESC[01;31m\ESC[Kasparagus\ESC[m\ESC[K"
      expected = "I have an apple, and \ESC[01;31m\ESC[Kyou\ESC[m\ESC[K have two. I have a bracelet and an \ESC[01;31m\ESC[Kaspar\ESC[m\ESC[K"
      actual = truncate_colorized_line 65 orgText
  in
    expected @=? actual
