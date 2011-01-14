#!/usr/bin/env ruby

if ARGV.length != 1 then
  puts "usage: fibon.rb <logfile>"
  exit 1
end

logFile  = ARGV.first
if not (File.exists?(logFile)) then
  puts "ERROR: fibon log file: '#{logFile}' does not exist"
  exit 1
end

commands = `grep ^@ #{logFile} | sed 's/^@//'`
puts commands
