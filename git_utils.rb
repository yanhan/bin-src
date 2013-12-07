module GitUtils
  # returns true if current directory is a git repository, false otherwise
  def self.in_git_repo?
    `git rev-parse 2>/dev/null`
    $? == 0
  end

  def self.exit_if_not_in_git_repo(programName = "")
    if !in_git_repo?
      $stderr.puts "#{programName}: fatal - not in a git repository"
      abort
    end
  end

  # Given a `grep` colorized line in `git grep -n` format, and the number of
  # columns of the terminal, compute the longest prefix of the colorized line
  # that can fit onto the terminal
  def self.colorized_line_truncate_to_fit(line, termCols)
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
end
