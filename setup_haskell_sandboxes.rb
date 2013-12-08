#!/usr/bin/env ruby

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
  HaskellPackage.new("hakyll", "4.4.2.0"),
  HaskellPackage.new("yesod", "1.2.4"),
  HaskellPackage.new("yesod-bin", "1.2.5.1")
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

def setup_haskell_sandboxes(homeDir)
  Dir.chdir(homeDir)
  puts "Calling 'cabal update'..."
  `cabal update`
  sandboxDir = File.join(homeDir, "haskellsandbox")
  if File.exists?(sandboxDir) && !File.directory?(sandboxDir)
    exit_fatal("#{sandboxDir} is a file. Please remove it to continue")
  elsif !File.exists?(sandboxDir)
    Dir.mkdir(sandboxDir)
  end
  FileUtils.chmod(0700, sandboxDir)
  packagesInstalled = []
  packagesAlreadyThere = []
  packagesFailedDueToExistingFile = []
  HASKELL_PACKAGES.each do |hpkg|
    Dir.chdir(sandboxDir)
    packageName = hpkg.get_package_dir
    if !File.exists?(packageName)
      Dir.mkdir(packageName)
      FileUtils.chmod(0700, packageName)
      Dir.chdir(packageName)
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
        end
      rescue PTY::ChildExited
        # child exited
      end
      packagesInstalled.push(packageName)
    elsif File.directory?(packageName)
      packagesAlreadyThere.push(packageName)
    elsif File.file?(packageName)
      packagesFailedDueToExistingFile.push(packageName)
    end
  end

  puts "Done."
  packagesFailedDueToExistingFile.each do |packageName|
    msg = "Failed to install #{packageName}"
    msg << " (#{File.join(sandboxDir, packageName)} is a file)"
    puts msg
  end

  packagesAlreadyThere.each do |packageName|
    puts "#{packageName} was installed prior to this."
  end

  packagesInstalled.each do |packageName|
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
