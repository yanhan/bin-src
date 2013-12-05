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
end
