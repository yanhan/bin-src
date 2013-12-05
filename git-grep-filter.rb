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
require "git_utils"

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
  fnameRegex = /^(.*?:\d+:)(.*)/
  if !res.empty?
    if $stdout.tty?
      IO.popen("less -R", "w") do |less|
        myOS = `uname -a`
        if myOS.start_with?("Darwin")
          # need to `cat` each line to a temporary file,
          # then perform `grep` for colorization
          fileTemp = Tempfile.new("#{$0}")
          tempFilePath = fileTemp.path
          res.each do |line|
            fileTemp.close
            File.truncate(tempFilePath, 0)
            if line.start_with?("Binary file")
              less.puts line
            else
              matchObj = fnameRegex.match(line)
              if matchObj
                pfx = matchObj[1]
                File.open(tempFilePath, "w") do |f|
                  f.write(matchObj[2])
                end
                less.puts pfx + `grep --color='always' '#{grepRegex}' #{tempFilePath}`
              else
                less.puts line
              end
            end
          end
        else
          res.each do |line|
            less.puts line
          end
        end
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
