
require (File.dirname(File.expand_path(__FILE__)) + '/_base')

class GraphCommand < Command
  def initialize
    @name       = "graph"
    @args       = ""
    @short_help = "produce a dot graph for each input file"
  end

  def parse(a)
    my_flags = ["--save"]
    @files = a.reject {|f| f =~ /^--/}
    @flags = a.select {|f| f =~ /^--/}.reject{|f| my_flags.member?(f)}
    @save  = a.member?("--save")
  end

  def run(outh)
    @files = $stdin.lines if (@files.nil? || @files == [])
    @files = @files.map{|f| f.chomp}
    gt_flags = @flags.join(' ')
    gt_exe   = "#{GT_EXE} #{gt_flags}"

    @files.each do |f|
      if not File.exists?(f) then
        outh.puts "File '#{f}' does not exist" unless 
        exit 1
      end
      exe = exe_name(f)
      out      = if @save then "> #{exe}.dot" else "" end
      outh.puts "#{f}"
      outh.flush

      if f.end_with?(".prep") then
        outh.puts `cat #{f} | #{gt_exe} #{out}`
      else
        outh.puts `#{CALVIN_EXE} graph-prep #{f} | #{gt_exe} #{out}`
      end
    end
  end
end

@commands << GraphCommand.new
