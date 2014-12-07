#!/usr/bin/env python

# Setup script for the entire project.
# For now, we only setup sbt

from __future__ import print_function

import datetime
import hashlib
import os
import os.path
import shutil
import sys

import requests

from config import *

_TEMPLATES_FOLDER = "templates"
_TEMPLATE_SBT_PATH = os.path.join(_TEMPLATES_FOLDER, "sbt")

_SBT_LAUNCH_JAR_PATH = os.path.join(BIN_DIR, "sbt-launch.jar")
_SBT_LAUNCH_JAR_URL = "https://repo.typesafe.com/typesafe/ivy-releases/org.scala-sbt/sbt-launch/0.13.7/sbt-launch.jar"
_SBT_PATH = os.path.join(BIN_DIR, "sbt")

def setup():
  """Setup everything"""
  _create_bin_dir()
  _install_sbt()
  if BIN_DIR not in os.environ["PATH"].split(":"):
    print(
      "BIN_DIR `{}` is not in the PATH environment variable\n"
      "Remember to add it to your PATH."
    )

def _create_bin_dir():
  """Creates the `BIN_DIR` if it does not exist"""
  if not os.path.exists(BIN_DIR):
    print("Creating BIN_DIR {} ...".format(BIN_DIR))
    os.makedirs(BIN_DIR, 0700)
  elif not os.path.isdir(BIN_DIR):
    _exit_with_message(
      "Path given in `BIN_DIR` {} exists but is not a directory.\n"
      "Please remove it manually and run this script again.".format(BIN_DIR)
    )

def _install_sbt():
  """Installs sbt. Based on instructions from:
  http://www.scala-sbt.org/0.13/tutorial/Manual-Installation.html"""
  print("Installing sbt-launch.jar ...")
  if not os.path.exists(_SBT_LAUNCH_JAR_PATH):
    # Download the sbt-launch.jar file.
    # From http://docs.python-requests.org/en/latest/user/quickstart/#raw-response-content
    _download_file(_SBT_LAUNCH_JAR_URL, _SBT_LAUNCH_JAR_PATH)
  elif not os.path.isfile(_SBT_LAUNCH_JAR_PATH):
    # `BIN_DIR/sbt-launch.jar` exists but is not a file
    _exit_with_message(
      "`{}` exists but is not a file. Please remove it manually and "
      "run this script again.".format(_SBT_LAUNCH_JAR_PATH)
    )
  else:
    checksums_same, actual_checksum = _same_sha256sum(_SBT_LAUNCH_JAR_PATH,
      SBT_LAUNCH_JAR_SHA256SUM
    )
    if not checksums_same:
      # `BIN_DIR/sbt-launch.jar` exists but it has a different checksum
      # Ask the user if he/she wants to download the `sbt-launch.jar` from our
      # hardcoded url
      qn = "File `{}`:\n- actual checksum {}\n- expected checksum {}\n" \
        "Download from {} ?".format(_SBT_LAUNCH_JAR_PATH, actual_checksum,
          SBT_LAUNCH_JAR_SHA256SUM, _SBT_LAUNCH_JAR_URL
        )
      if _prompt_to_do_action(qn):
        _backup_file(_SBT_LAUNCH_JAR_PATH)
        _download_file(_SBT_LAUNCH_JAR_URL, _SBT_LAUNCH_JAR_PATH)
  _exit_if_invalid_sha256sum(_SBT_LAUNCH_JAR_PATH, SBT_LAUNCH_JAR_SHA256SUM)

  # Install sbt script itself
  print("Installing sbt ...")
  if not os.path.exists(_SBT_PATH):
    shutil.copy(_TEMPLATE_SBT_PATH, _SBT_PATH)
    os.chmod(_SBT_PATH, 0700)
  elif not os.path.isfile(_SBT_PATH):
    _exit_with_message(
      "\"{}\" exists but is not a file. Please remove it manually and "
      "run this script again.".format(_SBT_PATH)
    )
  else:
    checksums_same, actual_checksum = _same_sha256sum(_SBT_PATH, SBT_SHA256SUM)
    if not checksums_same:
      # `BIN_DIR/sbt` exists but it has a different checksum from what we
      # expect. Ask the user if he/she wants a copy of `sbt` from our templates
      qn = "File `{}`:\n- actual checksum {}\n- expected checksum {}\n" \
        "Replace with provided `{}` file?".format(_SBT_LAUNCH_JAR_PATH,
          actual_checksum, SBT_SHA256SUM, _TEMPLATE_SBT_PATH
        )
      if _prompt_to_do_action(qn):
        _backup_file(_SBT_PATH)
        shutil.copy(_TEMPLATE_SBT_PATH, _SBT_PATH)
        os.chmod(_SBT_PATH, 0700)
  _exit_if_invalid_sha256sum(_SBT_PATH, SBT_SHA256SUM)
  print("Done.")

def _prompt_to_do_action(qn):
  """Repeatedly asks the user for a question until a y/yes/n/no answer is
  given; returns True if the user answers 'y', returns False if the user answers
  'n'"""
  prompt_string = "{} [y/yes/n/no] ".format(qn)
  while True:
    s = raw_input(prompt_string)
    s_lower = s.lower()
    if s_lower == "y" or s_lower == "yes":
      return True
    elif s_lower == "n" or s_lower == "no":
      return False
    print("Invalid answer. Accepted values: y, yes, n, no")

def _backup_file(filename):
  """Renames a given filename so it has a `-YYYY-MM-DD-HHMMSS` suffix"""
  dt_now = datetime.datetime.now()
  move_filename = "{}-{}-{}-{}-{}{:02d}{:02d}".format(filename, dt_now.year,
    dt_now.month, dt_now.day, dt_now.hour, dt_now.minute, dt_now.second
  )
  print("Backing up \"{}\" to \"{}\"".format(filename, move_filename))
  shutil.move(filename, move_filename)

def _exit_if_invalid_sha256sum(filename, expected_sha256sum):
  """Exits if the given file has a different SHA256SUM from its expected
  SHA256sum"""
  checksums_same, actual_sha256sum = _same_sha256sum(filename,
    expected_sha256sum
  )
  if not checksums_same:
    _exit_with_message(
      "`{}`\n- actual SHA256sum: {}\nexpected SHA256sum: {}".format(
        filename, actual_sha256sum, expected_sha256sum
      )
    )

def _same_sha256sum(filename, expected_sha256sum):
  """Returns (True, actual sha256sum) if the given file has the same SHA256SUM
  as the expected SHA256sum, returns (False, actual sha256sum) otherwise"""
  m = hashlib.sha256()
  with open(filename, "r") as f:
    m.update(f.read())
  actual_sha256sum = m.hexdigest()
  return (actual_sha256sum == expected_sha256sum, actual_sha256sum)

def _exit_with_message(msg):
  """Prints a given message and exits."""
  print(msg, file=sys.stderr)
  print("Exiting.", file=sys.stderr)
  sys.exit(1)

def _download_file(url, destination_file):
  """Downloads a file from a url and saves it into the destination file."""
  r = requests.get(url)
  with open(destination_file, "wb") as f:
    for chunk in r.iter_content(1024):
      f.write(chunk)

if __name__ == "__main__":
  setup()
