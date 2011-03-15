#!/usr/bin/env ruby
#
# Process the output from DynamoRIO's pcprofile
# pcprofile data is collected by passing the -prof_pcs option to drrun
#
# For example:
# $ drrun -ops -prof_pcs ls
#
require 'find'
require 'pp'
require 'ostruct'
require 'optparse'


###################################################################
#
# Data Types
#
###################################################################
class Location
  # keep all in array to enforce a standard output order
  @@all = [ 
    :wHERE_APP, 
    :wHERE_INTERP, 
    :wHERE_DISPATCH, 
    :wHERE_MONITOR,
    :wHERE_SYSCALL_HANDLER, 
    :wHERE_SIGNAL_HANDLER, 
    :wHERE_TRAMPOLINE,
    :wHERE_CONTEXT_SWITCH, 
    :wHERE_IBL, 
    :wHERE_FCACHE, 
    :wHERE_UNKNOWN]
  @@names = {
    :wHERE_APP               => "App",
    :wHERE_INTERP            => "Interp",
    :wHERE_DISPATCH          => "Dispatch",
    :wHERE_MONITOR           => "Monitor",
    :wHERE_SYSCALL_HANDLER   => "Syscall_Handler",
    :wHERE_SIGNAL_HANDLER    => "Signal_Handler",
    :wHERE_TRAMPOLINE        => "Trampoline",
    :wHERE_CONTEXT_SWITCH    => "Context_Switch",
    :wHERE_IBL               => "IBL",
    :wHERE_FCACHE            => "Fragment_Cache",
    :wHERE_UNKNOWN           => "Unknown"
  }
  def self.all; @@all end
  def self.name(loc); @@names[loc] end
end

class Sample 
  @@merge_count = 0
  attr_accessor *Location.all
  attr_accessor :exe_name
  attr_reader   :file_path, :fcache

  def initialize(file, name=nil)
    @file_path  = file
    parent_dir  = File.basename(File.dirname(file))
    @exe_name = name || parent_dir.each_char.take_while{|c| c != '.'}.join
    Location.all.each{|k| self.send(k.to_s+"=", 0)}
    @fcache = FragmentCache.new
  end

  def merge(other)
    s = Sample.new("<merged>", @exe_name)
    s.merge!(self)
    s.merge!(other)
  end

  def merge!(other)
    Location.all.each{|k| self.send(k.to_s+"=", self.send(k) + other.send(k))}
    other.fcache.values.each do |frag|
      #avoid improper fragment merge
      frag_id = frag.frag_id.to_s + "_" + @@merge_count.to_s
      add_fragment(frag_id, frag.count, frag.frag_type)
    end
    @@merge_count += 1
    self
  end

  def num_samples
    Location.all.map{|k| self.send(k)}.reduce(0, :+)
  end

  def dump_raw_data(outh=$stdout, options = {})
    sep = options[:sep] || "\t"
    dump_header = options[:dump_header]
    columns = [:exe_name]   + Location.all + [:num_samples]
    header  = (["Benchmark"] + Location.all.map {|l| Location.name(l)} + ["Total"])
    outh.puts header.join(sep) if dump_header
    outh.puts columns.map{|s| self.send(s)}.join(sep)
  end

  def dump_summary(outh=$stdout)
    non_zero = Location.all.reject{|l| self.send(l) == 0}
    total    = num_samples
    outh.puts "#{@exe_name} (#{total} total samples)"
    non_zero.each do |loc| 
      samples = self.send(loc)
      percent = (samples.to_f / total.to_f) * 100
      name    = Location.name(loc)
      outh.puts sprintf " %5.1f%% of time in %s (%d)", percent, name, samples

      if loc == :wHERE_FCACHE then
        on_trace    = @fcache.num_trace_fragments
        off_trace   = @fcache.num_bb_fragments
        p_on  = (on_trace.to_f   / samples.to_f) * 100
        p_off = (off_trace.to_f / samples.to_f) * 100
        outh.puts sprintf "\t%5.1f%% %3s a trace (%d)", p_on,  "in", on_trace 
        outh.puts sprintf "\t%5.1f%% %3s a trace (%d)", p_off, "off", off_trace 
      end
    end
  end

  def dump_trace_counts(outh=$stdout, options = {})
    sep = options[:sep] || "\t"
    counts = @fcache.values.map {|frag| frag.count}.sort.reverse
    outh.puts "#{@exe_name}#{sep}#{counts.join(sep)}"
  end

  def add_fragment(frag_id, count, frag_type)
    @fcache.add_fragment(frag_id, count, frag_type)
  end

  class FragmentCache
    def initialize 
      @fragments = {} 
    end
    def add_fragment(frag_id, count, frag_type)
      @fragments[frag_id] ||= Fragment.new(frag_id, frag_type)
      @fragments[frag_id].incr(count)
    end
    def num_trace_fragments
      count_fragments_by {|f| f.frag_type == :trace}
    end
    def num_bb_fragments
      count_fragments_by {|f| f.frag_type != :trace}
    end
    def count_fragments_by(&blk)
      @fragments.values.find_all(&blk).map{|t| t.count}.reduce(0,:+)
    end
    def keys
      @fragments.keys
    end
    def values
      @fragments.values
    end
    def [](i)
      @fragments[i]
    end
    class Fragment
      attr_reader :frag_id, :frag_type, :count
      def initialize(fid,ftype) @frag_id = fid; @frag_type = ftype; @count = 0 end
      def incr(amnt) @count += amnt end
    end
  end
end


###################################################################
#
# Processing Functions
#
###################################################################
def find_sample_files(root)
  sample_files = []
  Find.find(root) do |path|
    sample_files << path if File.basename(path) =~ /^pcsamples[.]*/
  end
  sample_files.sort
end

def build_sample(file)
  s = Sample.new(file)
  start = false
  lines = File.open(file).drop_while{|line| line !~ /^PC PROFILING RESULTS/}
  lines.drop(1).each do |line| #drop the PC PROFILING RESULTS LINE
    case line
      when /pc=0x[[:xdigit:]]+\s+#=(\d+)\s+in the app/
        s.wHERE_APP += $1.to_i
      
      when /pc=0x[[:xdigit:]]+\s+#=(\d+)\s+in (trace|fragment)\s+@(0x[[:xdigit:]]+)/
        cnt    = $1.to_i
        f_type = $2.to_sym
        f_id   = $3.hex
        s.wHERE_FCACHE += cnt
        s.add_fragment(f_id, cnt, f_type)
        
      when /pc=0x[[:xdigit:]]+\s+#=(\d+)\s+in DynamoRIO (<?[\w_]+(?: \w+)?>?)/
        amt   = $1.to_i
        where = $2
        incr  = 
          case where 
            when "interpreter"     then :wHERE_INTERP
            when "dispatch"        then :wHERE_DISPATCH
            when "monitor"         then :wHERE_MONITOR
            when "signal handler"  then :wHERE_SIGNAL_HANDLER
            when "syscall handler" then :wHERE_SYSCALL_HANDLER
            when "context switch"  then :wHERE_CONTEXT_SWITCH
            when "indirect_branch_lookup" then :wHERE_IBL
            else :wHERE_UNKNOWN
          end
        s.send(incr.to_s+"=", s.send(incr) + amt)
      else
        puts "UNKNOWN CATEGORY: " + line
    end
  end
  s
end

def merge_samples(samples, options)
  if options.merge then
  samples.group_by {|s| s.exe_name}.map do |exe,ss|
    ss.reduce(Sample.new("<merged>", exe), :merge)
  end.sort_by {|s| s.exe_name}
  else
    samples
  end
end

def dump_samples(samples, options)
  samples =
  if options.spec then 
    samples.map {|s| s.exe_name = rename_SPEC(s.exe_name); s}.sort_by {|s| sort_SPEC(s.exe_name)}
  else
    samples
  end

  first = true
  samples.each do |s|
    case options.output_type
      when :summary then s.dump_summary(options.outh)
      when :raw     then s.dump_raw_data(options.outh, :dump_header => first)
      when :trace   then s.dump_trace_counts(options.outh)
    end
    first = false
  end
end

def rename_SPEC(exe)
  base = exe.chars.take_while {|c| c != "_"}.join
  case base
    when "perlbench"  then "400.perlbench"
    when "bzip2"      then "401.bzip2"
    when "gcc"        then "403.gcc"
    when "mcf"        then "429.mcf"
    when "gobmk"      then "445.gobmk"
    when "hmmer"      then "456.hmmer"
    when "sjeng"      then "458.sjeng"
    when "libquantum" then "462.libquantum"
    when "h264ref"    then "464.h264ref"
    when "omnetpp"    then "471.omnetpp"
    when "astar"      then "473.astar"
    when "xalancbmk"  then "483.xalancbmk"
    when "specrand"   then "999.specrand"
    when "bwaves"     then "410.bwaves"
    when "gamess"     then "416.gamess"
    when "milc"       then "433.milc"
    when "zeusmp"     then "434.zeusmp"
    when "gromacs"    then "435.gromacs"
    when "cactusADM"  then "436.cactusADM"
    when "leslie3d"   then "437.leslie3d"
    when "namd"       then "444.namd"
    when "dealII"     then "447.dealII"
    when "soplex"     then "450.soplex"
    when "povray"     then "453.povray"
    when "calculix"   then "454.calculix"
    when "GemsFDTD"   then "459.GemsFDTD"
    when "tonto"      then "465.tonto"
    when "lbm"        then "470.lbm"
    when "wrf"        then "481.wrf"
    when "sphinx"     then "482.sphinx3"
    when "sphinx3"    then "482.sphinx3"
    when "specrand"   then "998.specrand"
    else base
  end
end

def sort_SPEC(name)
  case name 
    when "400.perlbench" then "_SPEC-Integer-400"
    when "401.bzip2"     then "_SPEC-Integer-401"
    when "403.gcc"       then "_SPEC-Integer-403"
    when "429.mcf"       then "_SPEC-Integer-429"
    when "445.gobmk"     then "_SPEC-Integer-445"
    when "456.hmmer"     then "_SPEC-Integer-456"
    when "458.sjeng"     then "_SPEC-Integer-458"
    when "462.libquantum"then "_SPEC-Integer-462"
    when "464.h264ref"   then "_SPEC-Integer-464"
    when "471.omnetpp"   then "_SPEC-Integer-471"
    when "473.astar"     then "_SPEC-Integer-473"
    when "483.xalancbmk" then "_SPEC-Integer-483"
    when "999.specrand"  then "_SPEC-float-999"
    when "410.bwaves"    then "_SPEC-float-410"
    when "416.gamess"    then "_SPEC-float-416"
    when "433.milc"      then "_SPEC-float-433"
    when "434.zeusmp"    then "_SPEC-float-434"
    when "435.gromacs"   then "_SPEC-float-435"
    when "436.cactusADM" then "_SPEC-float-436"
    when "437.leslie3d"  then "_SPEC-float-437"
    when "444.namd"      then "_SPEC-float-444"
    when "447.dealII"    then "_SPEC-float-447"
    when "450.soplex"    then "_SPEC-float-450"
    when "453.povray"    then "_SPEC-float-453"
    when "454.calculix"  then "_SPEC-float-454"
    when "459.GemsFDTD"  then "_SPEC-float-459"
    when "465.tonto"     then "_SPEC-float-465"
    when "470.lbm"       then "_SPEC-float-470"
    when "481.wrf"       then "_SPEC-float-481"
    when "482.sphinx3"   then "_SPEC-float-482"
    when "998.specrand"  then "_SPEC-float-998"
    else name
  end
end

###################################################################
#
# Command Line Options
#
###################################################################
class CmdLineOptions
  def self.parse(args)
    options = OpenStruct.new
    options.output_type = :raw
    options.outh        = $stdout
    options.merge       = true
    options.spec        = true

    opts = OptionParser.new do |opts|
      opts.banner = "Usage: pcprofile.rb [options] [file|dir]"
      opts.on("-t", "--type TYPE", [:raw, :summary, :trace], 
        "Output type (raw, summary, trace)") do |t|
        options.output_type = t
      end
      opts.on("-o", "--output FILE", "Set output file") do |o|
        options.outfile = o
      end
      opts.on("--no-merge", "Do not merge sample files") do |o|
        options.merge = false
      end
      opts.on("--no-spec", "Do not rename/sort SPEC exes") do |o|
        options.spec = false
      end
    end
    begin
      opts.parse!
    rescue
      puts opts
      exit 1
    end

    options
  end
end

###################################################################
#
# Main
#
###################################################################
if __FILE__ == $0 then
  # parse command line arguments
  options = CmdLineOptions.parse(ARGV)
  if options.outfile then
    begin 
      options.outh = File.open(options.outfile, "w") 
    rescue 
      puts "ERROR: unable to open file #{options.outfile} for writing" 
      exit 1 
    end
  end

  # process sample files
  files = []
  samples = []
  ARGV.each {|root| files += find_sample_files(root)}
  files.each do |f|
    samples << build_sample(f)
  end
  samples = merge_samples(samples, options)
  dump_samples(samples, options)
end

