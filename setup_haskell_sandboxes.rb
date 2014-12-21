#!/usr/bin/env ruby

# Used to install Haskell packages in cabal sandbox
#
# Assumptions:
# 1. You are using a `cabal` that supports the `sandbox` command.
#    Otherwise, run the `setup_cabal` script
# 2. $HOME environment variable is set, and you have read, write and
#    execute permissions
# 3. The directory storing the sandboxes will be at $HOME/haskellsandbox
#    Individual sandboxes are stored at $HOME/haskellsandbox/package-version ,
#    depending on the package and version.
# 4. Currently, to add new packages, append a new entry to the
#    `HASKELL_PACKAGES` constant below.

require "fileutils"
require "pty"
require "tmpdir"

class HaskellPackage
  attr_reader :name, :version
  def initialize(name, version)
    @name = name
    @version = version
  end

  def get_package_dir
    @name + "-" + @version
  end
end

# Yea... probably better to put this externally, but it will suffice
# for now
HASKELL_PACKAGES = [
  HaskellPackage.new("hakyll", "4.6.2.0"),
  HaskellPackage.new("yesod", "1.2.4"),
  HaskellPackage.new("yesod-bin", "1.2.5.1"),
  HaskellPackage.new("haddock", "2.10.0")
]

def exit_fatal(msg)
  puts "Fatal: #{msg}. Exiting."
  exit 1
end

def is_rwx?(path)
  File.readable?(path) && File.writable?(path) && File.executable?(path)
end

def insufficient_rwx_msg(path)
  "Insufficient permissions for #{path} " +
  "(Needs to be readable, writable, executable)"
end

def cabal_exists?
  `which cabal`
  $? == 0
end

def cabal_no_sandbox
  cwd = Dir.getwd
  hasSandbox = false
  Dir.mktmpdir do |dirname|
    Dir.chdir(dirname)
    `cabal sandbox init >/dev/null 2>&1`
    hasSandbox = ($? == 0)
  end
  Dir.chdir(cwd)
  !hasSandbox
end

# Takes an Array of HaskellPackage objects, and returns a Hash with keys
# "Installed" - already installed packages
# "ErrorFileExists" - a file with the same name as the package's destination
#                     directory exists
# "Pending" - Going to install
#
# Each key indexes into an array of strings (in 'package-version' format)
def classify_haskell_packages(sandboxDir, haskellPackages)
  ret = {}
  ret["Installed"] = []
  ret["ErrorFileExists"] = []
  ret["Pending"] = []
  ret["Error"] = []
  ret["Success"] = []
  haskellPackages.each do |hpkg|
    packageName = hpkg.get_package_dir
    packageDest = File.join(sandboxDir, packageName)
    if !File.exists?(packageDest)
      ret["Pending"].push(packageName)
    elsif File.directory?(packageDest)
      ret["Installed"].push(packageName)
    elsif File.file?(packageDest)
      ret["ErrorFileExists"].push(packageName)
    end
  end
  ret
end

def setup_haskell_sandboxes(homeDir)
  Dir.chdir(homeDir)
  sandboxDir = File.join(homeDir, "haskellsandbox")
  if File.exists?(sandboxDir) && !File.directory?(sandboxDir)
    exit_fatal("#{sandboxDir} is a file. Please remove it to continue")
  elsif !File.exists?(sandboxDir)
    Dir.mkdir(sandboxDir)
  end
  FileUtils.chmod(0700, sandboxDir)
  packagesClassified = classify_haskell_packages(sandboxDir, HASKELL_PACKAGES)
  if !packagesClassified["Pending"].empty?
    # only call cabal update if we have to install packages
    puts "Calling cabal update..."
    `cabal update`
  end
  packagesClassified["Pending"].each do |packageName|
    packageDest = File.join(sandboxDir, packageName)
    Dir.mkdir(packageDest)
    FileUtils.chmod(0700, packageDest)
    Dir.chdir(packageDest)
    puts "Installing #{packageName}"
    `cabal sandbox init`
    # Continuously print output of process, taken from
    # http://stackoverflow.com/questions/1154846/continuously-read-from-stdout-of-external-process-in-ruby/1162850#1162850
    begin
      PTY.spawn("cabal install #{packageName}") do |r, w, pid|
        begin
          r.each { |line| print line }
        rescue Errno::EIO
          # end of output
        end
        # wait for child process to exit to get exit code in $?
	Process.wait(pid)
      end
    rescue PTY::ChildExited
      # child exited
    end
    if $? == 0
      packagesClassified["Success"].push(packageName)
    else
      packagesClassified["Error"].push(packageName)
    end
  end

  puts "Done."
  packagesClassified["ErrorFileExists"].each do |packageName|
    msg = "Failed to install #{packageName}"
    msg << " (#{File.join(sandboxDir, packageName)} is a file;"
    msg << " remove it if you want to install #{packageName}"
    puts msg
  end

  packagesClassified["Error"].each do |packageName|
    puts "Failed to install '#{packageName}'"
  end

  packagesClassified["Installed"].each do |packageName|
    puts "#{packageName} was installed prior to this."
  end

  packagesClassified["Success"].each do |packageName|
    msg = "Successfully installed #{packageName}"
    msg << " (at #{File.join(sandboxDir, packageName)})"
    puts msg
  end
end

if !ENV.key?("HOME")
  exit_fatal("Fatal: $HOME not set")
end

homeDir = ENV["HOME"]
if !File.exists?(homeDir)
  exit_fatal("#{homeDir} does not exist")
elsif !File.directory?(homeDir)
  exit_fatal("#{homeDir} is not a directory")
elsif !is_rwx?(homeDir)
  exit_fatal(insufficient_rwx_msg(homeDir))
elsif !cabal_exists?
  exit_fatal("cabal does not exist. Please run the setup_cabal script")
elsif cabal_no_sandbox
  msg = "cabal does not have the `sandbox` command."
  msg << " Please update to a more modern version of cabal"
  exit_fatal(msg)
end

setup_haskell_sandboxes(homeDir)
