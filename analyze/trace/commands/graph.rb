
require (File.dirname(File.expand_path(__FILE__)) + '/_base')

class GraphCommand < Command
  def initialize
    @name       = "graph"
    @args       = ""
    @short_help = "produce a dot graph for each input file"
  end

  def parse(a)
    @files = a.clone
  end

  def run(outh)
    @files = $stdin.lines if (@files.nil? || @files == [])
    @files = @files.map{|f| f.chomp}

    @files.each do |f|
      if not File.exists?(f) then
        outh.puts "File '#{f}' does not exist" unless 
        exit 1
      end
      exe = exe_name(f)
      outh.puts "#{f}"
      outh.flush
      outh.puts `#{CALVIN_EXE} graph-prep #{f} | #{GT_EXE} > #{exe}.dot`
    end
  end
end

@commands << GraphCommand.new
