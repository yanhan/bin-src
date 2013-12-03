#!/usr/bin/env ruby

# git-grep-blame : combines `git grep -n` and `git blame`
#
# Used to find out the committer, commit SHA1, filename of lines in the
# repository that match a string we want to grep.
# Output is automatically piped to `less`
#
#
# Usage:
#
# ggb 'some_string'
# ggb '\bstring_with_regex\b'

def construct_blame_string(commitSHA1, commitTimeStamp, author, filename,
                           lineNum, textMatched)
  "#{commitSHA1} #{commitTimeStamp} #{author} #{filename}:#{lineNum} #{textMatched}"
end

def git_grep_blame(argv)
  if argv.empty?
    puts "#{$0}: please supply a string for git grep"
    exit(1)
  end
  arr = []
  # support 1 argument for now
  gitGrepOut = `git grep -n '#{argv[0]}'`
  gitGrepOut.split("\n").each do |line|
    matchObj = /^(.*?):(\d+):(.*)$/.match(line)
    fname = matchObj[1]
    lineNum = matchObj[2].to_i
    textMatched = matchObj[3]
    gitBlameResult = `git blame -L#{lineNum},#{lineNum} '#{fname}'`
    # capture SHA1 hash, committer name, commit timestamp
    gitBlameResult.split("\n").each do |gbLine|
      mObj = /^([0-9a-fA-F]{1,40}) \((.*)?(\d{4}-\d{2}-\d{2} \d{2}:\d{2}:\d{2} [\+\-]?\d{4})/.match(gbLine)
      if not mObj.nil?
        commitSHA1 = mObj[1]
        author = mObj[2]
        commitTimeStamp = mObj[3]
        arr.push(
          construct_blame_string(commitSHA1, commitTimeStamp, author, fname,
                                lineNum, textMatched)
        )
      end
    end
  end

  IO.popen("less", "w") do |f|
    arr.each do |line|
      f.puts line
    end
  end

end

git_grep_blame ARGV
