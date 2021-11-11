#!/usr/bin/env ruby
# ruby script to detect changes in test-related files
require 'yaml'
require 'digest'

hash   = Digest::SHA256.hexdigest("xyz")

filename= ARGV[0]

if !filename
  puts "Usage: #{$0} filename"
  exit 1
end

if !File.file? filename
  warn "Error. #{filename} doesn't exist." 
  exit 1
end

mismatch = 0
count = 0

test_files = YAML.load_file(filename)
test_files.each do |test_file|
  count = count + 1
  # file still exists?
  if !File.file? test_file['filename']
    warn "Error. Provided test file `#{test_file['filename']}` doesn't exist."
    mismatch = mismatch + 1
  end

  # check sha256 hash
  sha256 = Digest::SHA256.hexdigest(File.read(test_file['filename']))
  if test_file['sha256'] != sha256
    warn "Looks like #{test_file['filename']} has been modified as its SHA256 `#{sha256}` is not the same as the one in the record: #{test_file['sha256']}"
    mismatch = mismatch + 1
  else
    # no change to the test file
  end
end

if mismatch > 0
  warn "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
  warn "There are #{mismatch} mismatch. Please review the test files to ensure it has not been deleted/regenerated."
  warn "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
  exit 1
else
  puts "OK. All matched!"
end

if count == 0
  warn "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
  warn "Looks like the test file list in #{filename} is empty! Please check!"
  warn "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
  exit 1
end

