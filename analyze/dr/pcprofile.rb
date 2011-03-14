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

$usage = "pcprofile [dir]"

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
  attr_accessor *Location.all
  attr_reader   :file_path, :exe_name, :fcache

  def initialize(file)
    @file_path  = file
    parent_dir  = File.basename(File.dirname(file))
    @exe_name = parent_dir.each_char.take_while{|c| c != '.'}.join
    Location.all.each{|k| self.send(k.to_s+"=", 0)}
    @fcache = FragmentCache.new
  end

  def num_samples
    Location.all.map{|k| self.send(k)}.reduce(0, :+)
  end

  def dump_raw_data(outh, options = {})
    sep = options[:sep] || "\t"
    dump_header = options[:dump_header]
    columns = [:exe_name]   + Location.all + [:num_samples]
    header  = (["Benchmark"] + Location.all.map {|l| Location.name(l)} + ["Total"])
    outh.puts header.join(sep) if dump_header
    outh.puts columns.map{|s| self.send(s)}.join(sep)
  end

  def dump_summary(outh)
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

  def dump_trace_counts(outh, options = {})
    sep = options[:sep] || "\t"
    counts = @fcache.values.map {|frag| frag.count}.sort.reverse
    outh.puts "#{@exe_name}#{sep}#{counts.join(sep)}"
  end

  def add_fragment(frag_id, count, frag_type)
    @wHERE_FCACHE += count
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

class CmdLineOptions
  def self.parse(args)
    options = OpenStruct.new
    options.output_type = :raw
    options.outh        = $stdout

    opts = OptionParser.new do |opts|
      opts.banner = "Usage: pcprofile.rb [options] [file|dir]"
      opts.on("--type TYPE", [:raw, :summary, :trace], 
        "Output type (raw, summary, trace)") do |t|
        options.output_type = t
      end
      opts.on("-o", "--output FILE", "Set output file") do |o|
        options.outfile = o
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

if __FILE__ == $0 then
  if ARGV.length == 0 then
    puts $usage
    exit 1
  end
  
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
  ARGV.each {|root| files += find_sample_files(root)}
  first = true
  files.each do |f|
    s = build_sample(f)
    case options.output_type
      when :summary then s.dump_summary(options.outh)
      when :raw     then s.dump_raw_data(options.outh, :dump_header => first)
      when :trace   then s.dump_trace_counts(options.outh)
    end
    first = false
  end
end

