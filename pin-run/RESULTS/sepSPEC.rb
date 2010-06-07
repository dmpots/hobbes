#!/usr/bin/ruby
require 'pp'
require 'fileutils'

specFP = %w(
    410.bwaves
    416.gamess
    433.milc
    434.zeusmp
    435.gromacs
    436.cactusADM
    437.leslie3d
    444.namd
    447.dealII
    450.soplex
    453.povray
    454.calculix
    459.GemsFDTD
    465.tonto
    470.lbm
    481.wrf
    482.sphinx3
    998.specrand
)
SpecFPMatch = specFP.join("|")

specInt = %w(
    400.perlbench
    401.bzip2
    403.gcc
    429.mcf
    445.gobmk
    456.hmmer
    458.sjeng
    462.libquantum
    464.h264ref
    471.omnetpp
    473.astar
    483.xalancbmk
    999.specrand
)
SpecIntMatch = specInt.join("|")
SpecCompilers = %w(gcc icc llvm)


def specIntDir(specDir) 
  specDir.gsub("spec.", "spec.int.")
end
def specFpDir(specDir) 
  specDir.gsub("spec.", "spec.fp.")
end

if __FILE__ == $0 then
  $cleanOnly = false
  if ARGV.find{|a| a =~ /--clean/} then
    $cleanOnly = true
  end

  dirs = []
  SpecCompilers.each do |c|
    dirs  << Dir["spec.#{c}.*"]
  end
  dirs = dirs.flatten
  #pp dirs

  dirs.each do |specDir| 
    specFpDest  = specFpDir(specDir)
    specIntDest = specIntDir(specDir)
    dests = [specFpDest, specIntDest]
    dests.each do |dest|
      if File.exists?(dest) then
        FileUtils.rm_r(dest)
      end
    end
  end
  if($cleanOnly) then 
      puts "Only cleaning."
      exit 0 
  end

  dirs.each do |specDir| 
    specFpDest  = specFpDir(specDir)
    specIntDest = specIntDir(specDir)
    FileUtils.mkdir(specFpDest)
    FileUtils.mkdir(specIntDest)
    #pp "FP: #{specFpDest}"
    #pp "IT: #{specIntDest}"
    intFiles = []
    fpFiles  = []

    files = Dir["#{specDir}/*.LOG"]
    files.each do |file|
      if    file =~ /#{SpecIntMatch}/ then
        intFiles << file
      elsif file =~ /#{SpecFPMatch}/ then
        fpFiles << file
      end
    end

    #puts "INT FILES:"
    #pp intFiles
    #puts "FP  FILES:"
    #pp fpFiles

    puts "Copying #{intFiles.length} files from #{specDir} to #{specIntDest}"
    intFiles.each do |f|
      FileUtils.cp(f, specIntDest)
    end
    puts "Copying #{fpFiles.length} files from #{specDir} to #{specFpDest}"
    fpFiles.each do |f|
      FileUtils.cp(f, specFpDest)
    end
  end
end





