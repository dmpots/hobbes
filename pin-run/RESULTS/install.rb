#!/usr/bin/ruby
require 'pp'
require 'fileutils'

$cleanOnly = false
if(ARGV.grep(/--clean/).length > 0) then
  $cleanOnly = true
end
$exts = %w(opcodemix bblengthmix)
$extsRE = $exts.join("|")

def expandDir(d) 
  $exts.map{|ext| Dir["#{d}/*.#{ext}"]}.flatten
end

# Clean old files
files = expandDir(".")
if files.length > 0 then
  puts "Found current files: "
  pp files
  print "Really delete these directories? (Y/N) "
  r = $stdin.gets
  if r =~ /y|yes/i then
    puts "Deleting files"
    FileUtils.rm_rf(files)
  end
end

# Copy new files
srcDirs = ARGV.reject{|a| a =~ /--clean/}
if $cleanOnly || srcDirs.empty? then
  exit 0
end

srcDirs = srcDirs.map {|d| 
  if d =~ /\.#{$extsRE}$/ then
    d
  else
    expandDir(d)
  end
}.flatten

puts "Copying files from #{srcDirs.pretty_inspect}"
FileUtils.cp_r(srcDirs, ".")




