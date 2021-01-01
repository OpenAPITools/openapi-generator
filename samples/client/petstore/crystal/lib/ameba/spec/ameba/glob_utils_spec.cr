require "../spec_helper"

module Ameba
  struct GlobUtilsClass
    include GlobUtils
  end

  subject = GlobUtilsClass.new
  current_file_basename = File.basename(__FILE__)
  current_file_path = "spec/ameba/#{current_file_basename}"

  describe GlobUtils do
    describe "#find_files_by_globs" do
      it "returns a file by globs" do
        subject.find_files_by_globs(["**/#{current_file_basename}"])
          .should eq [current_file_path]
      end

      it "returns files by globs" do
        subject.find_files_by_globs(["**/*_spec.cr"])
          .should contain current_file_path
      end

      it "doesn't return rejected globs" do
        subject
          .find_files_by_globs(["**/*_spec.cr", "!**/#{current_file_basename}"])
          .should_not contain current_file_path
      end

      it "doesn't return duplicated globs" do
        subject
          .find_files_by_globs(["**/*_spec.cr", "**/*_spec.cr"])
          .count(current_file_path)
          .should eq 1
      end
    end

    describe "#expand" do
      it "expands globs" do
        subject.expand(["**/#{current_file_basename}"])
          .should eq [current_file_path]
      end

      it "does not list duplicated files" do
        subject.expand(["**/#{current_file_basename}", "**/#{current_file_basename}"])
          .should eq [current_file_path]
      end
    end
  end
end
