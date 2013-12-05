#!/usr/bin/env ruby

# git-grep-filter : git-grep -n , but chains grep -v to remove lines with
#                   unwanted keywords
#
# Usage:
# git-grep-filter 'needle' [<removeWord1> ...]
#
# Examples:
#
# Searches for 'needle', but discard lines with either 'Binary', 'spec' or 'rb'
#   git-grep-filter 'needle' 'Binary' 'spec' 'rb'
#
# Just like normal git grep -n:
#   git-grep-filter 'needle'

$: << File.join(ENV["HOME"], "bin")

require "git_utils"

def git_grep_filter argv
  if argv.empty?
    $stderr.puts "#{$0}: please supply a string for git-grep"
    abort
  end

  cmdString = "git grep -n #{argv[0]}"
  argv.shift
  argv.each do |line|
    cmdString << " | grep -v '#{line}'"
  end

  res = `#{cmdString}`.split("\n")
  if !res.empty?
    IO.popen("less", "w") do |less|
      res.each do |line|
        less.puts line
      end
    end
  end
end

GitUtils::exit_if_not_in_git_repo
git_grep_filter ARGV
