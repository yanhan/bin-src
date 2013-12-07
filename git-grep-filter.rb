#!/usr/bin/env ruby

# git-grep-filter : git-grep -n , but chains grep -v to remove lines with
#                   unwanted keywords
#
# Usage:
# git-grep-filter 'needle' [<wordToDiscard> ...]
#
# Examples:
#
# Searches for 'needle', but discard lines with either 'Binary', 'spec' or 'rb'
#   git-grep-filter 'needle' 'Binary' 'spec' 'rb'
#
# Just like normal git grep -n:
#   git-grep-filter 'needle'

$: << File.join(ENV["HOME"], "bin")

require "tempfile"
require "terminfo"
require "git_utils"

class GrepLine
  attr_reader :lineNum, :isBinary, :prefix, :grepText

  GREPLINE_REGEX = /^(.*?:\d+:)(.*)/
  BINARY_FILE_PREFIX = "Binary file"

  def initialize(lineNum, line)
    @lineNum = lineNum
    if line.start_with?(BINARY_FILE_PREFIX)
      @isBinary = true
      @prefix = line
    else
      @isBinary = false
      matchObj = GREPLINE_REGEX.match(line)
      if matchObj
        @prefix = matchObj[1]
        @grepText = matchObj[2]
      end
      # NOTE: Regex failure not handled
    end
  end
end

# Given a `grep` colorized line in `git grep -n` format, and the number of
# columns of the terminal, compute the longest prefix of the colorized line
# that can fit onto the terminal
def colorized_line_truncate_to_fit(line, termCols)
  # this regex is for `grep` colorized output, not `git-grep`
  colorRegex = /(\e\[[^m]*?m\e\[K)([^\e]*)(\e\[[^m]*m\e\[K)/
  lineLen = line.size
  if lineLen <= termCols
    return line
  end
  searchFromIdx = 0
  totalChars = 0
  matchObj = colorRegex.match(line, searchFromIdx)
  until matchObj.nil? do
    # chars before match
    numChars = matchObj.begin(0) - searchFromIdx
    if totalChars + numChars >= termCols
      stopIdx = searchFromIdx + termCols - totalChars - 1
      return line[0..stopIdx]
    end
    totalChars += numChars
    # chars between escape sequence
    numChars = matchObj[2].size
    if totalChars + numChars >= termCols
      stopIdx = matchObj.begin(2) + termCols - totalChars - 1
      return line[0..stopIdx] + matchObj[3]
    else
      totalChars += numChars
      searchFromIdx = matchObj.end(0)
    end
    matchObj = colorRegex.match(line, searchFromIdx)
  end
  # no more escape sequences
  numChars = lineLen - searchFromIdx
  if totalChars + numChars <= termCols
    return line
  else
    numChars = termCols - totalChars
    return line[0..(searchFromIdx + numChars - 1)]
  end
end

def git_grep_filter argv
  if argv.empty?
    $stderr.puts "#{$0}: please supply a string for git-grep"
    abort
  end

  grepRegex = argv[0]
  cmdString = "git grep -n #{grepRegex}"
  argv.shift
  argv.each do |line|
    cmdString << " | grep -v '#{line}'"
  end

  res = `#{cmdString}`.split("\n")
  if !res.empty?
    if $stdout.tty?
      _, termCols = TermInfo.screen_size
      IO.popen("less -R", "w") do |less|
        # split every line into prefix and grepText
        grepLineArray = res.zip(1..res.size).map do |zipArr|
          GrepLine.new(zipArr[1], zipArr[0])
        end
        fileTemp = Tempfile.new("#{$0}")
        tempFilePath = fileTemp.path
        # `cat` the suffixes to the temporary file
        grepLineArray.each do |grepLine|
          if !grepLine.isBinary
            fileTemp.write(grepLine.grepText)
            fileTemp.write("\n")
          end
        end
        fileTemp.close
        # perform `grep` for colorization, and obtain its output, split by newline
        coloredOutput = `grep --color='always' '#{grepRegex}' '#{tempFilePath}'`
        coloredOutputArray = coloredOutput.split("\n")
        nonBinaryLines = 0
        grepLineArray.each do |grepLine|
          if grepLine.isBinary
            less.puts grepLine.prefix
          else
            line = "#{grepLine.prefix} #{coloredOutputArray[nonBinaryLines]}"
            truncatedLine = colorized_line_truncate_to_fit(line, termCols)
            less.puts truncatedLine
            nonBinaryLines += 1
          end
        end
        fileTemp.unlink
      end
    else
      res.each do |line|
        $stdout.puts line
      end
    end
  end
end

GitUtils::exit_if_not_in_git_repo
git_grep_filter ARGV
