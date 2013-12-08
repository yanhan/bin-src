$: << File.join(__FILE__, "../..")
require "tmpdir"
require "git_utils"

describe GitUtils do
  describe "colorized_line_truncate_to_fit" do
    it "should return the original line if it fits within the terminal width" do
      orgLine = "has 12 chars"
      line = GitUtils.colorized_line_truncate_to_fit(orgLine, 12)
      expect(line).to eq(orgLine)
    end

    it "should truncate a line longer than the terminal width" do
      orgLine = "she sells seashells down the seashore"
      line = GitUtils.colorized_line_truncate_to_fit(orgLine, 20)
      expect(line).to eq("she sells seashells ")
    end

    it "should ignore the the length of color escape sequences" do
      orgLine = "hey man, [01;31m[Kshort sentence[m[K here"
      orgLine << ", should not [01;31m[Ktruncate[m[K too much"
      orgLine << ", otherwise the function will be [01;31m[Kwrong[m[K"
      orgLine << ", total of 116 chars"
      line = GitUtils.colorized_line_truncate_to_fit(orgLine, 116)
      expect(line).to eq(orgLine)
    end

    it "should correctly take into account the characters before a color escape seq (one color seq)" do
      orgLine = "a very long sentence here that will be truncated"
      orgLine << " before the [01;31m[color escape sequence[m[K"
      line = GitUtils.colorized_line_truncate_to_fit(orgLine, 20)
      expect(line).to eq("a very long sentence")
    end

    it "should correctly take into account the characters before a color escape seq (multiple color seq)" do
      orgLine = "some text [01;31m[Khere[m[K, I just want to show"
      orgLine << " that [01;31m[Kthis[m[K is correct, as such,"
      orgLine << " having [01;31m[Kmultiple[m[K tests is"
      orgLine << " pretty much necessary."
      orgLine << " There is also the stuff here that will "
      orgLine << "[01;31m[Knot[m[K be inside."
      expectedLine = "some text [01;31m[Khere[m[K, I just want to show"
      expectedLine << " that [01;31m[Kthis[m[K is correct, as such,"
      expectedLine << " having [01;31m[Kmultiple[m[K tests is"
      expectedLine << " pretty much necessary."
      line = GitUtils.colorized_line_truncate_to_fit(orgLine, 114)
      expect(line).to eq(expectedLine)
    end

    it "should correctly take into account characters after all color escape sequences, truncating a long line" do
      orgLine = "woohoo, [01;31m[Khow cool is that[m[K, I just got a"
      orgLine << " pair of new shoes, and I [01;31m[Kwill[m[K be"
      orgLine << " wearing them to work. Oh hell I gotta truncate this portion"
      expectedLine = "woohoo, [01;31m[Khow cool is that[m[K, I just got a"
      expectedLine << " pair of new shoes, and I [01;31m[Kwill[m[K be"
      expectedLine << " wearing them to work."
      line = GitUtils.colorized_line_truncate_to_fit(orgLine, 93)
      expect(line).to eq(expectedLine)
    end

    it "should correctly take into account characters after all color escape sequences, not truncating a shorter line" do
      orgLine = "RSpec seems to be [01;31m[Kreally[m[K similar to"
      orgLine << " [01;31m[Kjasmine[m[K; the learning curve is"
      orgLine << " [01;31m[Kvastly reduced[m[K for jasmine users"
      orgLine << ", so much so that, I [01;31m[Kfeel[m[K that I'm"
      orgLine << " using the same framework"
      line = GitUtils.colorized_line_truncate_to_fit(orgLine, 158)
      expect(line).to eq(orgLine)
    end

    it "will truncate part of colorized text that exceeds width of terminal" do
      orgLine = "This test should truncate the text right [01;31m[Khere"
      orgLine << " without the stuff here.[m[K"
      expectedLine = "This test should truncate the text right"
      expectedLine << " [01;31m[Khere[m[K"
      line = GitUtils.colorized_line_truncate_to_fit(orgLine, 45)
      expect(line).to eq(expectedLine)
    end
  end

  describe "in_git_repo?" do
    it "should return true if we are in a git repository" do
      cwd = Dir.getwd
      Dir.mktmpdir do |dirPath|
        Dir.chdir(dirPath)
        `git init .`
        expect(GitUtils.in_git_repo?).to be(true)
        Dir.chdir(cwd)
      end
    end

    it "should return false if we are not in a git repository" do
      cwd = Dir.getwd
      Dir.mktmpdir do |dirPath|
        Dir.chdir(dirPath)
        expect(GitUtils.in_git_repo?).to be(false)
        Dir.chdir(cwd)
      end
    end
  end
end
