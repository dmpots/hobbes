#!/usr/bin/ruby

require (File.dirname(File.expand_path(__FILE__)) + '/_base')
class FindLogsCommand < Command
  def initialize
    @name       = "find-logs"
    @args       = "<dir>"
    @short_help = "find the primiary log files"
  end

  def parse(args)
    @dir = ARGV.shift
    raise UsageError if @dir.nil?
    if not File.exists?(@dir) then
      puts "Directory #{@dir} does not exist"
      raise UsageError 
    end
    if not File.directory?(@dir) then
      puts "#{@dir} is not a directory"
      raise UsageError 
    end
  end

  def run(outh)
    files = {}
    Dir[@dir+"/*.LOG"].each do |f|
      next if f =~ /\.symtab\.LOG$/
      exe = exe_name(f)
      if files[exe] then
        if File.size(files[exe]) < File.size(f) then files[exe] = f end
      else
        files[exe] = f
      end
    end

    outh.puts files.values.sort.join("\n")
  end
end

(@commands ||= []) << (FindLogsCommand.new)

if __FILE__ == $0 then
  c = FindLogsCommand.new

  begin
    c.parse(ARGV)
    c.run($stdout)
  rescue UsageError
    puts c.usage
    exit 1
  end
end

