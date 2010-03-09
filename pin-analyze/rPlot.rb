#!/usr/bin/ruby
# Generate violin plots of the accuracy results
require 'pp'
require 'optparse'

ACCURACY_REGEX = /^Accuracy = (\d+\.?\d*)%/i
RANDINDX_REGEX = /^RANDIndx = (\d+\.?\d*)%/i
$statRegex = ACCURACY_REGEX

def processFiles(files, options)
  min   = 100.0
  max   = 0.0
  means = []
  files.each do |file|
    File.open(file).lines.each do |line|
      if line =~ options[:statregex] then
        g = $1.to_f
        means << g
      end
    end
  end
  #pp means
  means
end



if __FILE__ == $0 then
options = {}
options[:title] = ""
options[:statregex] = ACCURACY_REGEX 
OptionParser.new do |opts|
  opts.banner = "Usage: rPlot.rb [options] FILE1 [FILE2 ...]"

  opts.on("-r", "--rand", "Compute summary of RandIndex") do |v|
    $statRegex = RANDINDX_REGEX
    options[:statregex] = RANDINDX_REGEX
  end

  opts.on("-t", "--title [TITLE]", "Use title for graph") do |t|
    options[:title] = t
  end
end.parse!

files = {}
inputFiles = ARGV
inputFiles.each do |file|
  ext = File.extname(file).gsub(/^\./, "")
  if ext =~ /HNGS|HNIS|HNHT|HNHP|HTGS|HTGT|GSHP|GSIS|GSGT/ then
    pinTool =
    case file 
      when /jumpmix/   
        "jumpmix" 
      when /opcodemix/ 
        "opcodemix"
      when /regmix/    
        "regmix"
      when /bblengthmix/    
        "bblengthmix"
    end

    key = "#{ext}-#{pinTool}"
    files[key] ||= []
    files[key] << file
  end
end

# Dump Data
files.sort_by{|n,_|n.split("-",1)[0]}.each do |name,resultFiles|
  outfileName = "PLOTS/#{name}.dat"
  puts "Writing data to #{outfileName}"
  means = processFiles(resultFiles, options)
  File.open(outfileName, 'w') do |f|
    f.puts means.join("\n")
  end
end

# Dump Script
File.open('PLOTS/plot.r', 'w') do |f|
  xs = []
  i = 0
  files.sort_by{|n,_|n.split("-",2)[0]}.each do |name,resultFiles|
    outfileName = "#{name}.dat"
    x = "x#{i}"; i += 1
    f.puts "#{x} <- scan(\"#{outfileName}\") / 100"
    xs << x
  end
  names = files.keys.map{|n| n.split("-",2)[0]}.map{|s| "\"#{s}\""}.sort.join(",")
  f.puts "names <- c(#{names})"
  f.puts "vioplot(#{xs.join(',')}, names=names, col=\"tomato\", colMed=\"grey\", pchMed=10,ylim=c(0,1))"
  f.puts "title(\"#{options[:title]}\",xlab=\"benchmarks\",ylab=\"accuracy\")"
  f.puts "xs <- list(#{xs.join(',')})"
  f.puts "names(xs) <- names"
  f.puts "stats <- t(sapply(xs, summary))"
  f.puts "print(stats)"
  f.puts "latex.table(stats, caption=\"#{options[:title]}\",label=\"tab:stats\",rowlabel=\"Benchmark\")"
end



end
