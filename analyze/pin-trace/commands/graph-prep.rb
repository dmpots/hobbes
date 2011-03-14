
require (File.dirname(File.expand_path(__FILE__)) + '/_base')

class GraphPrepCommand < Command
  def initialize
    @name       = "graph-prep"
    @args       = ""
    @short_help = "prepare a log file for the graph command"
  end

  def parse(a)
    @files = a.reject {|f| f =~ /^--/}
    @flags = a.select {|f| f =~ /^--/}
    @save  = @flags.member?("--save")
  end

  def run(outh)
    @files = $stdin.lines if (@files.nil? || @files == [])
    @files = @files.map{|f| f.chomp}

    @files.each do |f|
      if not File.exists?(f) then
        outh.puts "File '#{f}' does not exist" unless 
        exit 1
      end
      outh.puts "##{f}"; outh.flush
      cmd = "cat #{f} | awk '{print $3}' | awk -f #{AWK_DIR}/clean.awk | uniq -c"
      cout = outh
      if @save then
        exe  = exe_name(f)
        cout = File.open(exe+'.prep', "w")
        cout.puts "##{f}"
      end
      io = IO.popen(cmd)
      io.each {|l| cout.puts l}
      if @save then cout.close end
    end
  end
end

@commands << GraphPrepCommand.new
