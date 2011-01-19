
require (File.dirname(File.expand_path(__FILE__)) + '/_base')

class GraphPrepCommand < Command
  def initialize
    @name       = "graph-prep"
    @args       = ""
    @short_help = "prepare a log file for the graph command"
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
      outh.puts "##{f}"
      outh.flush
      io = IO.popen("cat #{f} | awk '{print $3}' | awk -f #{AWK_DIR}/clean.awk | uniq -c")
      io.lines.each {|l| outh.puts l}
    end
  end
end

@commands << GraphPrepCommand.new
