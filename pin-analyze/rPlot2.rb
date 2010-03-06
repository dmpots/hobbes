#!/usr/bin/ruby
# generate data and scripts for R barcharts
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

#
# SPEC + nofib
#
tools.each do |tool|
ways.each do |way|
cutoffs.each do |cutoff|
sep("SPEC + nofib #{tool.upcase} #{way} > #{cutoff}%")
sets = [
  "CProgram ../pin-run/RESULTS/LATEST/#{way}/spec.gcc.#{tool}",
  "HaskellProgram ../pin-run/RESULTS/LATEST/#{way}/nofib.#{tool}"
]
mar(20)
genDataAndScript(sets, "gshn_#{tool}", cutoff,
  "x=\"right\",inset=c(-0.7, -0.7),cex=0.6,ncol=4", 
  "Combined (SPEC + nofib) #{tool.capitalize} (> #{cutoff}% #{way})",
  "% of total"
)
end
end
end

#
# SPEC + parallel
#
ways.each do |way|
tools.each do |tool|
cutoffs.each do |cutoff|
sep("SPEC + parallel #{tool.upcase} #{way} > #{cutoff}%")
sets = [
  "CProgram ../pin-run/RESULTS/LATEST/#{way}/spec.gcc.#{tool}",
  "HaskellProgram ../pin-run/RESULTS/LATEST/#{way}/parallel.ghc.#{tool}"
]
mar(20)
genDataAndScript(sets, "gshp_#{tool}", cutoff,
  "x=\"right\",inset=c(-0.7, -0.7),cex=0.6,ncol=4", 
  "Combined (SPEC + parallel) #{tool.capitalize} (> #{cutoff}% #{way})",
  "% of total"
)
end
end
end

#
# SPEC + parallel + shootout
#
ways.each do |way|
tools.each do |tool|
cutoffs.each do |cutoff|
sep("SPEC + parallel + shootout #{tool.upcase} #{way} > #{cutoff}%")
sets = [
  "CProgram ../pin-run/RESULTS/LATEST/#{way}/spec.gcc.#{tool}",
  "ParallelGhc ../pin-run/RESULTS/LATEST/#{way}/parallel.ghc.#{tool}",
  "ShootoutGhc ../pin-run/RESULTS/LATEST/#{way}/shootout.ghc.#{tool}"
]
mar(20)
genDataAndScript(sets, "gshpht_#{tool}", cutoff,
  "x=\"right\",inset=c(-0.7, -0.7),cex=0.6,ncol=4", 
  "Combined (SPEC + parallel + shootout) #{tool.capitalize} (> #{cutoff}% #{way})",
  "% of total"
)
end
end
end

#
# nofib only
#
ways.each do |way|
tools.each do |tool|
cutoffs.each do |cutoff|
sep("nofib only #{tool.upcase} #{way} > #{cutoff}%")
sets = [
  "HaskellProgram ../pin-run/RESULTS/LATEST/#{way}/nofib.#{tool}"
]
if cutoff == "0" then
mar(20)
labelOpts = "x=\"right\",inset=c(-0.7, -0.7),cex=0.8,ncol=3"
else
mar(10)
labelOpts = "x=\"right\",inset=c(-0.2, -0.2),cex=0.9"
end

genDataAndScript(sets, "hn_#{tool}", cutoff,
  labelOpts,
  "Nofib #{tool.capitalize} (> #{cutoff}% #{way})",
  "% of total"
)
end
end
end

#
# SPEC only
#
ways.each do |way|
tools.each do |tool|
cutoffs.each do |cutoff|
sep("SPEC only #{tool.upcase} #{way} > #{cutoff}%")
sets = [
  "CProgram ../pin-run/RESULTS/LATEST/#{way}/spec.gcc.#{tool}"
]
if tools == "opcodemix" then
  mar(20)
  labelOpts="x=\"right\",inset=c(-0.7, -0.7),cex=0.6,ncol=4"
else
  mar(10)
  labelOpts="x=\"right\",inset=c(-0.2, -0.2),cex=0.6,ncol=4" 
end

genDataAndScript(sets, "gs_#{tool}", cutoff,
  labelOpts,
  "SPEC #{tool.capitalize} (> #{cutoff}% #{way})",
  "% of total"
)
end
end
end


#
# END
#
$out.flush()
puts `cat PLOTS/plot2.r`

