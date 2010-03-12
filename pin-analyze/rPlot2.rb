#!/usr/bin/ruby
# generate stacked barcharts for raw pin results
require 'FileUtils'

inputDir   = "../pin-run/RESULTS/LATEST"
$commonOptions = "col=cc,legend.text=TRUE,las=2,ylim=c(0,1.0),axisnames=FALSE"
stdCutoffs = %w(0 10)
$out = File.open('PLOTS/plot2.r', 'w')
$log = $stdout 

def rCmds(baseName, n, legendOptions, title, ylab, xlab)
  $out.puts "#{baseName} <- as.matrix(read.table(\"#{baseName}.dat\", header=TRUE, row.names=1))[,seq(2,#{n},2)]"
  $out.puts ["barplot(#{baseName}",
                "#{$commonOptions}",
                "args.legend=list(#{legendOptions})",
                "main=\"#{title}\"",
                "ylab=\"#{ylab}\"",
                "xlab=\"#{xlab}\")"].join(",")
end

def run(cmd)
  $log.puts cmd
  $log.puts `#{cmd}`
end

def chooseTrainingSets(sets)
  run("./chooseTrainingSets.pl 0 #{sets.join(' ')}")
end

def pinalyze(c)
  run("./pinalyze -g -f 0.#{c} PREDICT/*")
end

def cp(baseName, cutoff)
  src="__Pinalyze__.dat"
  dest="PLOTS/#{baseName}#{cutoff}.dat"
  $log.puts "copying #{src} to #{dest}"
  FileUtils.cp(src, dest)
end

def genDataAndScript(sets, baseName, cutoff, 
                     legendOptions, title, ylab, xlab="benchmarks")
  chooseTrainingSets(sets)
  pinalyze(cutoff)
  cp(baseName, cutoff)
  n = Dir['PREDICT/*.LOG'].length
  rCmds(baseName+cutoff, n * 2, legendOptions, title,ylab,xlab)
end

def sep(comment="")
  $out.puts "
#
# #{comment}
#
"
end

def mar(n)
  $out.puts "par(mar=c(8,4,4,#{n}))"
end


#
# SETTINGS
#
tools= %w(opcodemix bblengthmix)
ways= %w(dylib nodylib)
cutoffs= %w(0 10)

if __FILE__ == $0 then
  ARGV.each do |plotFile|
    puts plotFile
    f = File.open(plotFile)
    instance_eval(f.read)
  end
end

#
# END
#
$out.flush()
puts `cat PLOTS/plot2.r`

